#lang racket

(provide trce dbug info warn erro crit ftal
  map-hash-table ;; TODO Remove? Used internally now
  hashify
  hash-set-from-list ;; TODO Remove? Handled by filter-newest-to-hash
  hash-remove-predicate
  decrement-time-to-live filter-newest-to-hash prune-old-messages update-elevator
  prune-requests fold-buttons-into-elevators)

(require lens racket/fasl racket/hash racket/pretty rackunit rackunit/text-ui sha threading
  "data-structures.rkt")

;; Create custom loggers that pretty-writes to standard error
(define-syntax-rule (generate-loggers type ...)
  (begin
    (... (define-syntax-rule (type expr ...)
           (begin
             (pretty-write `(,(string->symbol (format "~a:" (symbol->string 'type))) expr = ,expr) (current-error-port)) ...))) ...))

(generate-loggers trce dbug info warn erro crit ftal)

;; Hash a datum by serialization and then sha256
(define (hashify message) (sha256 (s-exp->fasl message)))

;; Maps each value in a hash-table
(define (map-hash-table hash function)
  (foldl (lambda (x s) (hash-set s x (function (hash-ref s x)))) hash (hash-keys hash)))

;; Append to a hash-table using a list
;; TODO Remove? Handled by filter-newest-to-hash
(define-syntax hash-set-from-list
  (syntax-rules (in: update-with:)
    [(_ hash accessor in: list update-with: function)
      (foldl (lambda (x s) (hash-set s (accessor x) (function x))) hash list)]))

;; Remove all key-value pairs where (predicate value) is true
(define (hash-remove-predicate hash predicate)
  (foldl (lambda (x s) (if (predicate (hash-ref s x)) (hash-remove s x) s)) hash (hash-keys hash)))

(define (elevator-attributes-refresh state)
  (elevator-attributes state time-to-live (current-inexact-milliseconds)))

;; Take messages, make into hash-set of attributes with ttl reset
(define (filter-newest-to-hash messages)
  (foldl (lambda (c s)
      (cond [(not (hash-has-key? s (elevator-state-id (first c)))) (hash-set s (elevator-state-id (first c)) (elevator-attributes (first c) 3 (last c)))]
            [(> (last c) (elevator-attributes-timestamp (hash-ref s (elevator-state-id (first c))))) (hash-set s (elevator-state-id (first c)) (elevator-attributes (first c) 3 (last c)))]
            [else s]))
    (make-immutable-hash)
    messages))

;; Discard messages older than current from all-elevators
(define (prune-old-messages elevators all-elevators)
  (hash-union elevators all-elevators #:combine/key (lambda (k a b) (if (> (elevator-attributes-timestamp a) (elevator-attributes-timestamp b)) a b))))

;; Decrement all 'time-to-live's
(define (decrement-time-to-live elevators)
  (map-hash-table elevators (lambda (x)
    (lens-transform elevator-attributes-time-to-live-lens x sub1))))

;; Update our own current state
(define (update-elevator elevators this-elevator)
  (hash-set elevators (elevator-state-id this-elevator) (elevator-attributes-refresh this-elevator)))

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
      (('(#s(external-command down 0 1)) '(#s(external-command down 0 0)))        '(#s(external-command down 0 0)))

      (((for/list ([i (range 50 150)]) (internal-command i 0)) (for/list ([i 100]) (internal-command i 0)))
        (append (for/list ([i (range 100 150)]) (internal-command i 0)) (for/list ([i 100]) (internal-command i 0)))))))
(run-tests prune-requests-tests)


;; Add the button presses (both external and internal) to the current elevator's state. Then put the current elevator state into the hash-map of all-elevators
(define (fold-buttons-into-elevators buttons this-elevator all-elevators)
  (let* ([button-presses      buttons]
         [internal-requests   (filter internal-command? button-presses)]
         [this-elevator*      (lens-transform elevator-state-internal-requests-lens this-elevator (curry prune-requests internal-requests))]
         [external-requests   (filter external-command? button-presses)]
         [this-elevator**     (lens-transform elevator-state-external-requests-lens this-elevator* (curry prune-requests external-requests))]
         [all-elevators*      (hash-set all-elevators (elevator-state-id this-elevator**) (elevator-attributes this-elevator** time-to-live (current-inexact-milliseconds)))])
    (values this-elevator** all-elevators*)))
