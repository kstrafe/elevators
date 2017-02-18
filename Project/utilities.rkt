#lang racket

(provide (all-defined-out))

(require lens racket/fasl racket/hash racket/pretty racket/syntax rackunit rackunit/text-ui threading
  "data-structures.rkt" "motor.rkt" "logger.rkt" (for-syntax racket/syntax))

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
  (map-hash-table elevators (lambda (x) (lens-transform attributes-time-to-live-lens x sub1))))

;; Update our own current state
(define (insert-self-into-elevators elevators folded-elevators)
  (lens-set this:state elevators (lens-view this:state folded-elevators)))

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
(define call-of-hash (lens-compose state-call-requests-lens attributes-state-lens))
(define (unify-requests all-elevators)
  (~>
    (unify-requests* all-elevators done-of-hash)
    (unify-requests* call-of-hash)))

(define (prune-call-requests-that-are-done all-elevators)
  (let* ([state        (first (hash-values all-elevators))]
         [done         (lens-view done-of-hash state)]
         [external     (lens-view call-of-hash state)]
         [external*    (foldr (lambda (x s) (if (ormap (curry equal? x) done) s (cons x s))) empty external)])
    (map-hash-table all-elevators (lambda (x)
      (lens-set call-of-hash x external*)))))

;; Add the button presses (both external and internal) to the current elevator's state. Then put the current elevator state into the hash-map of all-elevators
(define (fold-buttons-into-elevators buttons elevators)
  (let* ([command-requests   (filter command-request? buttons)]
         [elevators* (lens-transform this:command elevators (curry prune-requests command-requests))]
         [call-requests   (filter call-request? buttons)]
         [elevators** (lens-transform this:call elevators* (curry prune-requests call-requests))])
    elevators**))

(define (make-empty-elevator id name)
  (attributes-refresh (state id name 0 empty empty empty empty 0)))

(define (update-position hash)
  (let ([floor (any-new-floor-reached?)])
    (if floor
      (lens-set this:position hash floor)
      hash)))

(define (compute-available-call-requests hash)
  (define other:servicing (lens-compose state-servicing-requests-lens attributes-state-lens))
  (define other:call      (lens-compose state-call-requests-lens attributes-state-lens))
  (let ([elevators (hash-values hash)])
    (~>
      (map (curry lens-view other:servicing) elevators)
      flatten
      (remove* _ (flatten (map (curry lens-view other:call) elevators)))
      (filter call-request? _)
      flatten)))

(define (compute-direction-of-travel state)
  (compute-direction-to-travel state (state-servicing-requests state)))

(define (compute-direction-to-travel state requests)
  (if (empty? requests)
    'halt
    (let ([position (state-position state)]
          [request  (first requests)])
      (cond
        ([< position (request-floor request)] 'up)
        ([> position (request-floor request)] 'down)
        ; ([symbol=? (request-direction request) 'up] 'up)
        ; ([symbol=? (request-direction request) 'down] 'down)
        (else                                 (compute-direction-to-travel state (rest requests)))))))

(define (score-elevator-request state request)
  (abs (- (state-position state) (request-floor request))))

(define (id-score-sort id-score-1 id-score-2)
  (cond
    ([< (second id-score-1) (second id-score-2)] #t)
    ([> (second id-score-1) (second id-score-2)] #f)
    ([string<? (first id-score-1) (first id-score-2)] #t)
    ([string>=? (first id-score-1) (first id-score-2)] #f)))

(define (elevator-request-direction elevator)
  (let ([request (first-or-empty (state-servicing-requests elevator))])
    (if (empty? request)
      'halt
      (request-direction request))))

(define (process-available-call-requests hash requests)
  (define any:state attributes-state-lens)
  (let ([top-request (first-or-empty requests)])
    (if (empty? top-request)
      hash
      (begin
        (let ([best-id
            (~>
              (map (curry lens-view any:state) (hash-values hash))
              (map (lambda (x) (list x (compute-direction-of-travel x))) _)
              (map (lambda (x) (append x (list (compute-direction-to-travel (first x) (list top-request))))) _)
              (map (lambda (x) (append x (list (request-direction top-request)))) _)
              (map (lambda (x) (append x (list (elevator-request-direction (first x))))) _)
              trce*
              (filter
                (lambda (x)
                  (or
                    (and
                      (symbol=? (second x) (third x))
                      (symbol=? (second x) (fourth x)))
                    (symbol=? (second x) 'halt))) _)
              (map first _)
              (map (lambda (x) (list (state-id x) (score-elevator-request x top-request))) _)
              (sort id-score-sort)
              first-or-empty
              first-or-empty)])
          (process-available-call-requests
            (if (empty? best-id)
              hash
              (lens-transform (other:servicing best-id) hash (curry cons top-request)))
            (rest requests))
          )))))

(define (try-self-assign-external-task hash)
  (process-available-call-requests hash (compute-available-call-requests hash)))

(define (set-motor-direction-to-task! hash)
  ;; Calls move-to-floor depending on the top of the 'servicing-requests'
  ;; field in the current elevator.
  ;; If the current floor is the servicing floor, then
  ;; the motor will not start moving, so this function is seems to be alright
  ;; as it is.
  ;
  ;; The ! at the end of the function name denotes side-effect without altering hash.
  ;; Maybe we should in general separate side effects from changing the hash table.
  (let ([servicing-requests (lens-view this:servicing hash)])
    (when (not (empty? servicing-requests))
      (~>
        (first servicing-requests)
        request-floor
        move-to-floor)))
  hash)

(define (first-or-empty list) (if (empty? list) empty (first list)))

(define (sort-servicing hash)
  (let ([direction (compute-direction-of-travel (lens-view this:state hash))])
    (lens-transform this:servicing hash
      (lambda (x)
        (~>
          (if (symbol=? direction 'halt)
            x
            (sort x
              (cond
                ([symbol=? direction 'up] <)
                ([symbol=? direction 'down] >))
              #:key request-floor))
          )))
    ))

(define (prune-servicing-requests hash)
  (let ([servicing (first-or-empty (lens-view this:servicing hash))])
    (if (not (empty? servicing))
      (if (= (lens-view this:position hash) (request-floor servicing))
        ;; We know that the motor has tuned into this floor, so it's safe to remove (we're not in transit)
        (~>
          (lens-transform this:servicing hash rest)
          (lens-transform this:done      _ (lambda (done) (if (call-request? servicing) (cons servicing done) done)))
          (lens-transform this:command  _ (lambda (internal) (if (command-request? servicing) (remove servicing internal) internal)))
          (lens-set this:opening         _ 20)
          prune-servicing-requests)
        hash)
      hash)))

(define (remove-dead-elevators elevators)
  (hash-remove-predicate elevators (lambda (x) (<= (attributes-time-to-live x) 0))))
