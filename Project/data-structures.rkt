#lang racket

(provide (all-defined-out))

(require lens "identity-generator.rkt" "logger.rkt")

(define floor-count 4)
(define iteration-sleep-time 0.1)
(define time-to-live 6)

(define-syntax-rule (struct/lens-es (name (attributes ...) keywords ...) ...)
  (begin (struct/lens name (attributes ...) #:prefab keywords ...) ...))

(struct/lens-es
  (elevator-attributes (state time-to-live timestamp))
  (elevator-state      (id name position servicing-requests external-requests internal-requests done-requests resting-position opening-time))
  (external-command    (direction floor timestamp))
  (internal-command    (floor timestamp)))

;; Load/generate an identity
(define-values (id name) (generate-or-load-identity))
(info id name)

(define state-lens (lens-compose elevator-attributes-state-lens (hash-ref-lens id)))
(define opening-lens (lens-compose elevator-state-opening-time-lens elevator-attributes-state-lens (hash-ref-lens id)))
(define internal-requests-lens (lens-compose elevator-state-internal-requests-lens state-lens))
(define external-requests-lens (lens-compose elevator-state-external-requests-lens state-lens))
(define position-lens (lens-compose elevator-state-position-lens state-lens))
(define done-lens (lens-compose elevator-state-done-requests-lens state-lens))
(define servicing-lens (lens-compose elevator-state-servicing-requests-lens state-lens))
