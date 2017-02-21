#lang racket

(provide (all-defined-out))

(require racket/pretty racket/syntax)

;; Create custom loggers that pretty-writes to standard error
(define-syntax-rule (generate-loggers type ...)
  (begin
    (...
      (define-syntax-rule (type expr ...)
        (begin
          (pretty-write `(,(format-symbol "~a:" 'type) expr = ,expr) (current-error-port)) ...))) ...))

(generate-loggers trce dbug info warn erro crit ftal)
