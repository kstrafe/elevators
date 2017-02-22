#lang racket

(provide invoker)

(require "core-unit.rkt" "identity-generator-unit.rkt" "utilities-unit.rkt" "logger.rkt")

;; Propagate invocation of the main function to core unit
(define (invoker complex-struct)
  ;; Invoke units
  (define-values/invoke-unit/infer identity-generator@)
  (define-values/invoke-unit/infer utilities@)
  (define-values/invoke-unit/infer core@)
  (core complex-struct))
