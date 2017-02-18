#lang racket

(provide
  trce dbug info warn erro crit ftal
  prune-requests-that-are-done
  trce* dbug* info* warn* erro* crit* ftal*
  compute-servicing-of-internal-requests
  compute-the-task-to-take
  set-motor-direction-to-task!
  update-position
  remove-tasks-that-motor-completed
  hashify
  hash-remove-predicate
  elevator-attributes-refresh
  unify-requests
  make-empty-elevator
  decrement-time-to-live filter-newest-to-hash prune-old-messages update-elevator
  prune-requests fold-buttons-into-elevators)

(require lens racket/fasl racket/hash racket/pretty racket/syntax rackunit rackunit/text-ui sha threading
  "data-structures.rkt" "motor.rkt"
  (for-syntax racket/syntax))

(define-syntax (generate-stream-logger syn)
  (let* ([f (cdr (syntax->datum syn))]
         [n (for/list ([i f]) (format-symbol "~a*" i))])
    (datum->syntax syn
      `(begin ,@(for/list ([i n])
        `(define-syntax-rule (,i expr)
          (begin
            (let ([e expr])
              (pretty-write (list ',i '_ '= e) (current-error-port))
              e))))))))

;; Create custom loggers that pretty-writes to standard error
(define-syntax-rule (generate-loggers type ...)
  (begin
    (...
      (define-syntax-rule (type expr ...)
        (begin
          (pretty-write `(,(format-symbol "~a:" 'type) expr = ,expr) (current-error-port)) ...))) ...))

;; TODO: Clean this mess up into a single macro call!
(generate-loggers trce dbug info warn erro crit ftal)
(generate-stream-logger trce dbug info warn erro crit ftal)

;; Hash a datum by serialization and then sha256
(define (hashify message) (sha256 (s-exp->fasl message)))

;; Maps each value in a hash-table
(define (map-hash-table hash function)
  (foldl (lambda (x s) (hash-set s x (function (hash-ref s x)))) hash (hash-keys hash)))

;; Remove all key-value pairs where (predicate value) is true
(define (hash-remove-predicate hash predicate)
  (foldl (lambda (x s) (if (predicate (hash-ref s x)) (hash-remove s x) s)) hash (hash-keys hash)))

(define (elevator-attributes-refresh state)
  (elevator-attributes state time-to-live (current-inexact-milliseconds)))

;; Take messages, make into hash-set of attributes with ttl reset
(define (filter-newest-to-hash messages)
  (foldl (lambda (c s)
    (let* ([state (first c)]
           [id (elevator-state-id state)]
           [timestamp (last c)]
           [elev-ts elevator-attributes-timestamp])
      (if (or (not (hash-has-key? s id)) (> timestamp (elev-ts (hash-ref s id))))
        (hash-set s id (elevator-attributes state time-to-live timestamp))
        s)))
    (make-immutable-hash)
    messages))

;; Discard messages older than current from all-elevators
(define (prune-old-messages elevators all-elevators)
  (hash-union elevators all-elevators #:combine (lambda (a b) (if (> (elevator-attributes-timestamp a) (elevator-attributes-timestamp b)) a b))))

;; Decrement all 'time-to-live's
(define (decrement-time-to-live elevators)
  (map-hash-table elevators (lambda (x)
    (lens-transform elevator-attributes-time-to-live-lens x sub1))))

;; Update our own current state
(define (update-elevator elevators this-elevator)
  (let ([current-elevator (this-elevator elevators)])
    (hash-set elevators (elevator-state-id current-elevator) (elevator-attributes-refresh current-elevator))))

(define (equal-command? left right)
  (or
    (and (internal-command? left) (internal-command? right)
         (= (internal-command-floor left) (internal-command-floor right)))
    (and (external-command? left) (external-command? right)
         (symbol=?
           (external-command-direction left) (external-command-direction right))
         (= (external-command-floor left) (external-command-floor right)))))

;; Unify new and stored -commands into a list where new-commands that already exist in stored-commands are excluded
(define (prune-requests new-commands stored-commands)
  (~>
    (foldl
      (lambda (x s)
        (if (ormap (curry equal-command? x) stored-commands)
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
      (('(#s(external-command up 0 0)) '(#s(external-command up 0 0)))            '(#s(external-command up 0 0)))
      (('(#s(external-command up 1 0)) '(#s(external-command up 1 0)))            '(#s(external-command up 1 0)))
      (('(#s(external-command up 1 2)) '(#s(external-command up 1 0)))            '(#s(external-command up 1 0)))
      (('(#s(external-command up 1 0)) '(#s(external-command up 1 2)))            '(#s(external-command up 1 2)))

      (('(#s(external-command down 0 0)) '(#s(external-command up 0 0)))          '(#s(external-command down 0 0) #s(external-command up 0 0)))
      (('(#s(external-command up 0 0)) '(#s(external-command down 0 0)))          '(#s(external-command up 0 0) #s(external-command down 0 0)))
      (('(#s(external-command down 0 1)) '(#s(external-command down 0 0)))        '(#s(external-command down 0 0)))

      (((for/list ([i (range 50 150)]) (internal-command i 0)) (for/list ([i 100]) (internal-command i 0)))
        (append (for/list ([i (range 100 150)]) (internal-command i 0)) (for/list ([i 100]) (internal-command i 0)))))))
(run-tests prune-requests-tests)

(define (unify-requests* all-elevators accessor)
  (~>
    (map (curry lens-view accessor) (hash-values all-elevators))
    flatten
    remove-duplicates
    (sort > #:key external-command-timestamp)
    (define unified-dones _))
  (map-hash-table all-elevators (lambda (x)
    (lens-set accessor x unified-dones))))

(define done-of-hash (lens-compose elevator-state-done-requests-lens elevator-attributes-state-lens))
(define external-of-hash (lens-compose elevator-state-external-requests-lens elevator-attributes-state-lens))
(define (unify-requests all-elevators)
  (~>
    (unify-requests* all-elevators done-of-hash)
    (unify-requests* external-of-hash)))

(define (prune-requests-that-are-done all-elevators)
  (let* ([state        (first (hash-values all-elevators))]
         [done         (lens-view done-of-hash state)]
         [external     (lens-view external-of-hash state)]
         [external*    (foldr (lambda (x s) (if (ormap (curry equal? x) done) s (cons x s))) empty external)])
    (map-hash-table all-elevators (lambda (x)
      (lens-set external-of-hash x external*)))))

;; Add the button presses (both external and internal) to the current elevator's state. Then put the current elevator state into the hash-map of all-elevators
(define (fold-buttons-into-elevators buttons this-elevator elevators)
  (let* ([button-presses      buttons]
         [internal-requests   (filter internal-command? button-presses)]
         [this-elevator*      (lens-transform elevator-state-internal-requests-lens (this-elevator elevators) (curry prune-requests internal-requests))]
         [external-requests   (filter external-command? button-presses)]
         [this-elevator**     (lens-transform elevator-state-external-requests-lens this-elevator* (curry prune-requests external-requests))]
         [all-elevators*      (hash-set elevators (elevator-state-id this-elevator**) (elevator-attributes this-elevator** time-to-live (current-inexact-milliseconds)))])
    all-elevators*))

(define (make-empty-elevator id name)
  (elevator-attributes-refresh (elevator-state id name 0 empty empty empty empty 0 0)))

(define (update-position hash lens)
  (let ([floor (any-new-floor-reached?)])
    (if floor
      (lens-set lens hash floor)
      hash)))

(define (compute-the-task-to-take hash this-elevator id)
  ;; Simply move the top task from external to pending
  ;; TODO Implement better algorithm, this is currently useless except for demonstrations
  ;; TODO Don't use 'this-elevator', use lenses instead
  (let* ([current-elevator (this-elevator hash)]
         [external         (elevator-state-external-requests current-elevator)]
         [servicing        (elevator-state-servicing-requests current-elevator)])
    (~>
      (if (empty? servicing)
        (if (not (empty? external))
          (~>
            (lens-set elevator-state-servicing-requests-lens current-elevator (list (first external)))
            (lens-transform elevator-state-external-requests-lens _ rest))
          current-elevator)
        current-elevator)
      (define elevator _))
    (hash-set hash id (elevator-attributes-refresh elevator))))

(define (get-direction position servicing-requests opening)
  (if (empty? servicing-requests)
    0
    (let ([command (first servicing-requests)])
      (cond
        [(external-command? command) (sgn (- (external-command-floor command) position))]
        [(internal-command? command) (sgn (- (internal-command-floor command) position))]))))

(define (compute-servicing-of-internal-requests hash position-lens servicing-lens internal-requests-lens opening-lens)
  (let ([position (lens-view position-lens hash)]
        [servicing-requests (lens-view servicing-lens hash)]
        [internal-requests (lens-view internal-requests-lens hash)]
        [opening (lens-view opening-lens hash)])
    (trce servicing-requests internal-requests)
    ;; Find direction of travel based on position, servicing and door
    (let ([direction (get-direction position servicing-requests opening)])
      (trce direction)
      (if (= direction 0)
        ;; TODO Put this request into servicing of the current hash and return it for further use
        (let ([oldest-internal-request (foldl (lambda (c s) (if (> (internal-command-timestamp c) (internal-command-timestamp s)) c s)) (first internal-requests) (rest internal-requests))])
          (trce internal-requests))
        #f)
      hash)))

(define (command-floor command)
  (cond
    [(external-command? command) (external-command-floor command)]
    [(internal-command? command) (internal-command-floor command)]))

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
        command-floor
        move-to-floor)))
  hash)

(define (remove-tasks-that-motor-completed hash this-elevator id)
  ;; TODO Remove all tasks where the position of the elevator is equal to a task
  ;; TODO Rename this function
  hash)
