#lang racket

;;;; Provides loggers to standard error ports. The loggers automatically write the expressions too
;;;; This means writing `(trce (+ 1 2 3))` writes `(trce: (+ 1 2 3) = 6)`.
;;;; Appending a star to the logger name prevents writing the expression: `(trce* (+ 1 2 3)` writes
;;;; `(trce*: _ = 6)`.

(provide (except-out (all-defined-out) make-loggers))

(require racket/syntax (for-syntax racket/syntax syntax/parse))

(define-syntax (make-loggers stx)
  (syntax-parse stx
    [(_ name:id ...+)
      (with-syntax ([(rename ...) (for/list ([name-e (syntax-e #'(name ...))]) (format-id stx "~a*" name-e))])
        #'(begin
          (begin
            (...
              (define-syntax-rule (name expr ...)
                (begin
                  (pretty-write `(,(format-symbol "~a:" 'name) expr = ,expr) (current-error-port)) ...)))
            (define (rename expr)
              (pretty-write `(,(format-symbol "~a:" 'rename) _ = ,expr) (current-error-port))
              expr)) ...))]))

(make-loggers trce dbug info warn erro crit ftal)
