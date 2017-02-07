#! /usr/bin/env racket
#lang racket

(require lens
         threading
         "identity-generator.rkt"
         "network/network.rkt"
         "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(struct/lens elevator-state (id name position) #:prefab)
(struct/lens elevator-attributes (state time-to-live timestamp) #:prefab)

(define-values (id name) (generate-identity))

(define state (elevator-state id name 0))

(dbug id name)

(let loop ([my-friends (make-immutable-hash `([,id . ,(elevator-attributes state 3 (current-inexact-milliseconds))]))])
  (send state)
  (sleep 1)
  (let ([messages (receive)])
    (trce messages)
    (~>
      (foldl (lambda (x s) (hash-set s x (lens-transform elevator-attributes-time-to-live-lens (hash-ref s x) sub1)))
             my-friends
             (hash-keys my-friends))
      (foldl (lambda (x s) (hash-set s (elevator-state-id x) (elevator-attributes x 3 (current-inexact-milliseconds))))
             _
             messages)
      (loop))))
