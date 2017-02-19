#lang racket

(provide (all-defined-out))

(require racket/pretty racket/syntax
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

;; TODO Clean this mess up into a single macro call!
(generate-loggers trce dbug info warn erro crit ftal)
(generate-stream-logger trce dbug info warn erro crit ftal)
