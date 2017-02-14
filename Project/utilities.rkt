#lang racket

(provide trce dbug info warn erro crit ftal
  map-hash-table hashify hash-set-from-list hash-remove-predicate)

(require racket/fasl racket/pretty sha)

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

;; Filter messages, leaving only the newest message
(define (filter-newest this-elevator all-elevators messages)
  (~>
    ;; Filter all messages on id and newest time
    (foldl (lambda (c s)
        (cond [(not (hash-has-key? s (elevator-state-id (first c)))) (hash-set s (elevator-state-id (first c)) (elevator-attributes (first c) 3 (last c)))]
              [(> (last c) (elevator-attributes-timestamp (hash-ref s (elevator-state-id (first c))))) (hash-set s (elevator-state-id (first c)) (elevator-attributes (first c) 3 (last c)))]
              [else s]))
      (make-immutable-hash)
      messages)
    ;; Discard messages older than current from all-elevators
    (hash-union all-elevators #:combine/key (lambda (k a b) (if (> (elevator-attributes-timestamp a) (elevator-attributes-timestamp b)) a b)))
    ;; Update our own current state
    (hash-set id (elevator-attributes this-elevator 3 (current-inexact-milliseconds)))))
