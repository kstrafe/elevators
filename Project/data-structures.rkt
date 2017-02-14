#lang racket

(provide (all-defined-out))

(require lens)

(define time-to-live 3)
(struct/lens elevator-state (id name position servicing-requests external-requests internal-requests done-requests resting-position opening-time) #:prefab)
(struct/lens elevator-attributes (state time-to-live timestamp) #:prefab)
