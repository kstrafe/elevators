#lang racket

(provide invoker)

(require "core-unit.rkt" "identity-generator-unit.rkt" "utilities-unit.rkt"
   "logger.rkt")

;; Invoke units
(define-values/invoke-unit/infer identity-generator@)
(define-values/invoke-unit/infer utilities@)
(define-values/invoke-unit/infer core@)

;; Propagate invocation of the main function to core unit
(define (invoker complex-struct)
  ;; TODO Consider moving invoking here
  (core complex-struct))
