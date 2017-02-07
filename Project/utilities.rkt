#lang racket

(provide trce dbug info warn erro crit ftal)

(define-syntax-rule (generate-loggers type ...)
  (begin
    (... (define-syntax-rule (type expr ...)
           (begin
             (writeln `(,(string->symbol (format "~a:" (symbol->string 'type))) expr = ,expr)) ...))) ...))

(generate-loggers trce dbug info warn erro crit ftal)
