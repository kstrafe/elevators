#lang racket

(provide trce dbug info warn erro crit ftal
         map-hash-table
         hash-set-from-list
         hash-remove-predicate)

(require racket/pretty)

;; Create custom loggers that pretty-writes to standard error
(define-syntax-rule (generate-loggers type ...)
  (begin
    (... (define-syntax-rule (type expr ...)
           (begin
             (pretty-write `(,(string->symbol (format "~a:" (symbol->string 'type))) expr = ,expr) (current-error-port)) ...))) ...))

(generate-loggers trce dbug info warn erro crit ftal)

;; Maps each value in a hash-table
(define (map-hash-table hash function)
  (foldl (lambda (x s) (hash-set s x (function (hash-ref s x)))) hash (hash-keys hash)))

;; Append to a hash-table using a list
(define-syntax hash-set-from-list
  (syntax-rules (in: update-with:)
    [(_ hash accessor in: list update-with: function)
      (foldl (lambda (x s) (hash-set s (accessor x) (function x)))
        hash
        list)]))

;; Remove all key-value pairs where (predicate value) is true
(define (hash-remove-predicate hash predicate)
  (foldl (lambda (x s)
    (if (predicate (hash-ref s x))
      (hash-remove s x)
      s))
    hash
    (hash-keys hash)))
