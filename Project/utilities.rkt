#lang racket

(provide trce dbug info warn erro crit ftal hashify)

(require racket/fasl
         racket/pretty
         sha)

(define-syntax-rule (generate-loggers type ...)
  (begin
    (... (define-syntax-rule (type expr ...)
           (begin
             (pretty-write `(,(string->symbol (format "~a:" (symbol->string 'type))) expr = ,expr) (current-error-port)) ...))) ...))

(generate-loggers trce dbug info warn erro crit ftal)

(define (hashify message)
  (sha256 (s-exp->fasl message)))
