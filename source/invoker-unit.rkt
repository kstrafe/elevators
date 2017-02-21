#lang racket

(provide invoker-unit)

(require "invoker-sig.rkt" "core-unit.rkt" "utilities-unit.rkt"
  "identity-generator.rkt" "logger.rkt")

(define-unit invoker@
  (import)
  (export invoker^)

  (info "Invoker started")

  ;; Load/generate an identity
  (define-values (id name) (generate-or-load-identity#io))
  (info id name))

;; Invoke units
(define-values/invoke-unit/infer invoker@)
(define-values/invoke-unit/infer utilities@)
(define-values/invoke-unit/infer core@)

;; Propagate invocation of the main function to core unit
(define (invoker-unit complex-struct)
  (core complex-struct))
