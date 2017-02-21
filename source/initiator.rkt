#lang racket

(provide initiator)

(require "core.rkt" "core-sig.rkt" "identity-generator.rkt" "logger.rkt" "utilities.rkt" "utilities-sig.rkt")

;; Load/generate an identity
(define-values (id name) (generate-or-load-identity#io))
(info id name)

(define-values/invoke-unit/infer utilities@)
(define-values/invoke-unit/infer core@)

(define-compound-unit/infer initiator@
  (import)
  (export core^ utilities^)
  (link core@ utilities@))

(define (initiator complex-struct id name)
  (core complex-struct id name))
