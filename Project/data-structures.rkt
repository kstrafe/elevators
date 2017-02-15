#lang racket

(provide (all-defined-out))

(require lens)

(define floor-count 4)
(define time-to-live 6)
(define iteration-sleep-time 1)

(struct/lens elevator-attributes (state time-to-live timestamp) #:prefab)
(struct/lens elevator-state (id name position servicing-requests external-requests internal-requests done-requests resting-position opening-time) #:prefab)
(struct/lens external-command (direction floor timestamp) #:prefab)
(struct/lens internal-command (floor timestamp) #:prefab)
