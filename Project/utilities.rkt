#lang racket

(provide trce dbug info warn erro crit ftal
  map-hash-table hashify hash-set-from-list hash-remove-predicate
  prune-requests fold-buttons-into-elevators)

(require "data-structures.rkt" lens racket/fasl racket/pretty rackunit rackunit/text-ui sha threading)

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
(define-syntax hash-set-from-list
  (syntax-rules (in: update-with:)
    [(_ hash accessor in: list update-with: function)
      (foldl (lambda (x s) (hash-set s (accessor x) (function x))) hash list)]))

;; Remove all key-value pairs where (predicate value) is true
(define (hash-remove-predicate hash predicate)
  (foldl (lambda (x s) (if (predicate (hash-ref s x)) (hash-remove s x) s)) hash (hash-keys hash)))

;; Unify new and stored -commands into a list where new-commands that already exist in stored-commands are excluded
(define (prune-requests new-commands stored-commands)
  (~>
    ;; Filter the new-commands by removing all that have a match in stored-commands
    (foldl
      (lambda (x s)
        (if (ormap (lambda (y) (and (symbol=? (first x) (first y)) (= (second x) (second y)))) stored-commands)
          s
          (cons x s)))
      empty new-commands)
    reverse
    ;; Append the new commands to the stored commands
    (append stored-commands)))

(define-syntax-rule (check-equals? call ((input ...) result) ...)
  (begin (check-equal? (call input ...) result) ...))

(define prune-requests-tests
  (test-suite "Test prune-requests"
  (check-equals? prune-requests
    (('() '())                                              '())
    (('((up 0 0)) '((command 0 0)))                         '((up 0 0) (command 0 0)))
    (('((up 1 2) (down 3 4)) '((down 5 6)))                 '((up 1 2) (down 3 4) (down 5 6)))
    (('((up 1 2) (down 3 4)) '((down 3 6)))                 '((up 1 2) (down 3 6)))
    (('((command 0 0)) '())                                 '((command 0 0)))
    (('() '((command 0 0)))                                 '((command 0 0)))
    (('() '((command 1 0) (command 0 0)))                   '((command 1 0) (command 0 0)))
    (('((command 0 0)) '((command 0 1)))                    '((command 0 1)))
    (('((command 1 0)) '((command 0 1)))                    '((command 1 0) (command 0 1)))
    (('((command 2 100) (command 1 0)) '((command 0 1)))    '((command 2 100) (command 1 0) (command 0 1))))))
(run-tests prune-requests-tests)


;; Fold the button presses into the current elevator. Then put the current elevator into all-elevators
(define (fold-buttons-into-elevators buttons this-elevator all-elevators)
  (let* ([button-presses buttons]
         ;; Need to filter commands here
         [internal-requests (filter (lambda (x) (eq? (first x) 'command)) button-presses)]
         [this-elevator* (lens-set elevator-state-internal-requests-lens this-elevator (prune-requests internal-requests (elevator-state-internal-requests this-elevator)))]
         [external-requests (filter (lambda (x) (not (eq? (first x) 'command))) button-presses)]
         [this-elevator** (lens-set elevator-state-external-requests-lens this-elevator* (prune-requests external-requests (elevator-state-external-requests this-elevator*)))]
         [all-elevators* (hash-set all-elevators (elevator-state-id this-elevator**) (elevator-attributes this-elevator** time-to-live (current-inexact-milliseconds)))])
    (values this-elevator** all-elevators*)))
