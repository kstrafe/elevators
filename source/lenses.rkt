#lang racket

(provide (all-defined-out))

(require lens "data-structures.rkt" "identity-generator.rkt")

;; this: and other: lenses
;; A lens is a functional getter/setter/transformer
;; It's used to easily manipulate a value in a deeply nested data structure.
(define this:attribute (hash-ref-lens id))
(define this:ttl       (lens-compose attributes-time-to-live-lens this:attribute))
(define this:state     (lens-compose attributes-state-lens this:attribute))
(define this:opening   (lens-compose state-opening-time-lens this:state))
(define this:command   (lens-compose state-command-requests-lens this:state))
(define this:call      (lens-compose state-call-requests-lens this:state))
(define this:position  (lens-compose state-position-lens this:state))
(define this:done      (lens-compose state-done-requests-lens this:state))
(define this:servicing (lens-compose state-servicing-requests-lens this:state))
(define cplx:command   (lens-compose this:command complex-elevators-lens))

(define (other:servicing id) (lens-compose (lens-compose state-servicing-requests-lens attributes-state-lens (hash-ref-lens id))))

(define done-of-hash (lens-compose state-done-requests-lens attributes-state-lens))
(define call-of-hash (lens-compose state-call-requests-lens attributes-state-lens))
