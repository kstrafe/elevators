#! /usr/bin/env racket
#lang racket

(require lens
         "identity-generator.rkt"
         "network/network.rkt"
         "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(define-values (id name) (generate-identity))

(dbug id name)

(struct/lens elevator-state (name position) #:prefab)
(struct/lens elevator-attributes (state time-to-live timestamp) #:prefab)

(let loop ([my-friends (hash id (elevator-attributes (elevator-state name 0) 3 (current-inexact-milliseconds)))])
  (send `((string-append "Hi! My name is " ,name ", I'm also known as " ,id) ,id ,name ,(current-inexact-milliseconds)))
  (sleep 1)
  (trce my-friends)
  (trce
    (for/list ([message (receive)])
      (match message
        [(list _ id name time) (string-append "Yaaayy! Welcome " name " of " id " t=" (number->string time))]
        [_ (displayln "Unknown message :( I'm scared")])))
  (loop my-friends))
