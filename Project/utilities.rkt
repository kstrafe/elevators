#lang racket

(provide (all-defined-out))

(require lens racket/fasl racket/hash racket/pretty racket/syntax rackunit rackunit/text-ui threading
  "data-structures.rkt" "motor.rkt" "logger.rkt"
  (for-syntax racket/syntax))

;; Maps each value in a hash-table
(define (map-hash-table hash function)
  (foldl (lambda (x s) (hash-set s x (function (hash-ref s x)))) hash (hash-keys hash)))

;; Remove all key-value pairs where (predicate value) is true
(define (hash-remove-predicate hash predicate)
  (foldl (lambda (x s) (if (predicate (hash-ref s x)) (hash-remove s x) s)) hash (hash-keys hash)))

(define (attributes-refresh state)
  (attributes state time-to-live (current-inexact-milliseconds)))

;; Take messages, make into hash-set of attributes with ttl reset
;; TODO Clean up this dirty code if possible
(define (filter-newest-to-hash messages)
  (foldl (lambda (c s)
    (let* ([state (first c)]
           [id (state-id state)]
           [timestamp (last c)]
           [elev-ts attributes-timestamp])
      (if (or (not (hash-has-key? s id)) (> timestamp (elev-ts (hash-ref s id))))
        (hash-set s id (attributes state time-to-live timestamp))
        s)))
    (make-immutable-hash)
    messages))

;; Discard messages older than current from all-elevators
(define (unify-messages-and-elevators messages elevators)
  (let ([ts attributes-timestamp])
    (hash-union messages elevators #:combine (lambda (a b) (if (> (ts a) (ts b)) a b)))))

;; Decrement all 'time-to-live's
(define (decrement-time-to-live elevators)
  (map-hash-table elevators (lambda (x)
    (lens-transform attributes-time-to-live-lens x sub1))))

;; Update our own current state
(define (insert-self-into-elevators elevators folded-elevators)
  (lens-set state-lens elevators (lens-view state-lens folded-elevators)))

(define (same-request? left right)
  (and (symbol=? (request-direction left) (request-direction right))
       (= (request-floor left) (request-floor right))))

;; Unify new and stored -commands into a list where new-commands that already exist in stored-commands are excluded
(define (prune-requests new-commands stored-commands)
  (~>
    (foldl
      (lambda (x s)
        (if (ormap (curry same-request? x) stored-commands)
          s
          (cons x s)))
      empty new-commands)
    reverse
    (append stored-commands)))

(define-syntax-rule (check-equals? call ((input ...) result) ...)
  (begin (check-equal? (call input ...) result) ...))

(define prune-requests-tests
  (test-suite "Test prune-requests"
    (check-equals? prune-requests
      (('() '())                                                                  '())
      (('(#s(request up 0 0)) '(#s(request up 0 0)))            '(#s(request up 0 0)))
      (('(#s(request up 1 0)) '(#s(request up 1 0)))            '(#s(request up 1 0)))
      (('(#s(request up 1 2)) '(#s(request up 1 0)))            '(#s(request up 1 0)))
      (('(#s(request up 1 0)) '(#s(request up 1 2)))            '(#s(request up 1 2)))

      (('(#s(request down 0 0)) '(#s(request up 0 0)))          '(#s(request down 0 0) #s(request up 0 0)))
      (('(#s(request up 0 0)) '(#s(request down 0 0)))          '(#s(request up 0 0) #s(request down 0 0)))
      (('(#s(request down 0 1)) '(#s(request down 0 0)))        '(#s(request down 0 0)))

      (((for/list ([i (range 50 150)]) (request 'command i 0)) (for/list ([i 100]) (request 'command i 0)))
        (append (for/list ([i (range 100 150)]) (request 'command i 0)) (for/list ([i 100]) (request 'command i 0)))))))
(run-tests prune-requests-tests)

(define (unify-requests* all-elevators accessor)
  (~>
    (map (curry lens-view accessor) (hash-values all-elevators))
    flatten
    remove-duplicates
    (sort > #:key request-timestamp)
    (define unified-dones _))
  (map-hash-table all-elevators (lambda (x)
    (lens-set accessor x unified-dones))))

(define done-of-hash (lens-compose state-done-requests-lens attributes-state-lens))
(define external-of-hash (lens-compose state-external-requests-lens attributes-state-lens))
(define (unify-requests all-elevators)
  (~>
    (unify-requests* all-elevators done-of-hash)
    (unify-requests* external-of-hash)))

(define (prune-external-requests-that-are-done all-elevators)
  (let* ([state        (first (hash-values all-elevators))]
         [done         (lens-view done-of-hash state)]
         [external     (lens-view external-of-hash state)]
         [external*    (foldr (lambda (x s) (if (ormap (curry equal? x) done) s (cons x s))) empty external)])
    (map-hash-table all-elevators (lambda (x)
      (lens-set external-of-hash x external*)))))

;; Add the button presses (both external and internal) to the current elevator's state. Then put the current elevator state into the hash-map of all-elevators
(define (fold-buttons-into-elevators buttons elevators)
  (let* ([internal-requests   (filter command-request? buttons)]
         [elevators* (lens-transform internal-requests-lens elevators (curry prune-requests internal-requests))]
         [external-requests   (filter call-request? buttons)]
         [elevators** (lens-transform external-requests-lens elevators* (curry prune-requests external-requests))])
    elevators**))

(define (make-empty-elevator id name)
  (attributes-refresh (state id name 0 empty empty empty empty 0)))

(define (update-position hash lens)
  (let ([floor (any-new-floor-reached?)])
    (if floor
      (lens-set lens hash floor)
      hash)))

(define (compute-available-external-requests hash)
  (define servicing-lens (lens-compose state-servicing-requests-lens attributes-state-lens))
  (define external-lens  (lens-compose state-external-requests-lens attributes-state-lens))
  (let ([elevators (hash-values hash)])
    (~>
      (map (curry lens-view servicing-lens) elevators)
      flatten
      (filter call-request? _)
      (remove* _ (flatten (map (curry lens-view external-lens) elevators)))
      flatten)))

(define (compute-direction-of-travel state)
  (compute-direction-to-travel state (first-or-empty (state-servicing-requests state))))

(define (compute-direction-to-travel state request)
  (let ([position  (state-position state)])
    (cond
      ([empty? request]                     'halt)
      ([< position (request-floor request)] 'up)
      ([> position (request-floor request)] 'down)
      (else                                 'halt))))

(define (score-elevator-request state request)
  (abs (- (state-position state) (request-floor request))))

(define (id-score-sort id-score-1 id-score-2)
  (cond
    ([< (second id-score-1) (second id-score-2)] #t)
    ([> (second id-score-1) (second id-score-2)] #f)
    ([string<? (first id-score-1) (first id-score-2)] #t)
    ([string>=? (first id-score-1) (first id-score-2)] #f)))

(define (reverse-cons lst item)
  (append lst (list item)))

(define (process-available-external-requests hash requests)
  (define state-lens attributes-state-lens)
  (let ([top-request (first-or-empty requests)])
    (if (empty? top-request)
      hash
      (begin
        (let ([best-id
            (~>
              (map (curry lens-view state-lens) (hash-values hash))
              (map (lambda (x) (list x (compute-direction-of-travel x))) _)
              (map (lambda (x) (append x (list (compute-direction-to-travel (first x) top-request)))) _)
              (filter (lambda (x) (or (symbol=? (second x) (third x)) (symbol=? (second x) 'halt))) _)
              (map first _)
              (map (lambda (x) (list (state-id x) (score-elevator-request x top-request))) _)
              (sort id-score-sort)
              first-or-empty
              first-or-empty)])
          (process-available-external-requests
            (if (empty? best-id)
              hash
              (lens-transform (lens-compose state-servicing-requests-lens attributes-state-lens (hash-ref-lens best-id)) hash (lambda (x) (reverse-cons x top-request))))
            (rest requests))
          )))))

(define (try-self-assign-external-task hash)
  (process-available-external-requests hash (compute-available-external-requests hash)))

(define (set-motor-direction-to-task! hash lens)
  ;; Calls move-to-floor depending on the top of the 'servicing-requests'
  ;; field in the current elevator.
  ;; If the current floor is the servicing floor, then
  ;; the motor will not start moving, so this function is seems to be alright
  ;; as it is.
  ;
  ;; The ! at the end of the function name denotes side-effect without altering hash.
  ;; Maybe we should in general separate side effects from changing the hash table.
  (let ([servicing-requests (lens-view lens hash)])
    (when (not (empty? servicing-requests))
      (~>
        (first servicing-requests)
        request-floor
        move-to-floor)))
  hash)

(define (first-or-empty list)
  (if (empty? list)
    empty
    (first list)))

(define (less-than cutoff floor-1 floor-2)
  (cond
    ([<= floor-2 cutoff] #t)
    ([<= floor-1 cutoff] #f)
    (else (< floor-1 floor-2))))

(define (more-than cutoff floor-1 floor-2)
  (cond
    ([>= floor-2 cutoff] #t)
    ([>= floor-1 cutoff] #f)
    (else (> floor-1 floor-2))))

(define (sort-servicing hash servicing-lens state-lens)
  ; (dbug hash)
  (~>
    (let ([direction (compute-direction-of-travel (lens-view state-lens hash))])
      (lens-transform servicing-lens hash (lambda (x)
        ; (trce direction)
        (if (symbol=? direction 'halt)
          x
          (sort x
            (cond
              ;; This is correct, but we need to exclude floors above some levels :O
              ([symbol=? direction 'up] (curry less-than (state-position (lens-view state-lens hash))))
              ([symbol=? direction 'down] (curry more-than (state-position (lens-view state-lens hash)))))
            #:key request-floor)))))
    ;trce*
    ))

(define (prune-servicing-requests hash servicing-lens done-lens opening-lens pos-lens internal-lens)
  (let ([servicing (first-or-empty (lens-view servicing-lens hash))])
    (if (not (empty? servicing))
      (if (= (lens-view pos-lens hash) (request-floor servicing))
        ;; We know that the motor has tuned into this floor, so it's safe to remove (we're not in transit)
        (~>
          (lens-transform servicing-lens hash rest)
          (lens-transform done-lens      _ (lambda (done) (if (call-request? servicing) (cons servicing done) done)))
          (lens-transform internal-lens  _ (lambda (internal) (if (command-request? servicing) (remove servicing internal) internal)))
          (lens-set opening-lens         _ 20)
          (prune-servicing-requests servicing-lens done-lens opening-lens pos-lens internal-lens))
        hash)
      hash)))

(define (remove-dead-elevators elevators)
  (hash-remove-predicate elevators (lambda (x) (<= (attributes-time-to-live x) 0))))
