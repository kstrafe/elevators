#lang racket

(provide (all-defined-out))

(require lens)

(define floor-count 4)
(define time-to-live 6)
(define iteration-sleep-time 0.1)

(define-syntax-rule (struct/lens-es (name (attributes ...) keywords ...) ...)
  (begin (struct/lens name (attributes ...) #:prefab keywords ...) ...))

(struct/lens-es
  (elevator-attributes (state time-to-live timestamp))
  (elevator-state      (id name position servicing-requests external-requests internal-requests done-requests resting-position opening-time))
  (external-command    (direction floor timestamp))
  (internal-command    (floor timestamp)))
