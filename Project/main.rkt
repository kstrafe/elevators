#! /usr/bin/env racket
#lang racket

(require racket/pretty
         "identity-generator.rkt"
         "network/network.rkt"
         "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(define-values (id name) (generate-identity))

(dbug id name)

(let loop ([my-friends empty])
  (send `((string-append "Hi! My name is " ,name ", I'm also known as " ,id) ,id ,name ,(current-inexact-milliseconds)))
  (pretty-print
    (for/list ([message (receive)])
      (match message
        [(list _ id name time) (string-append "Yaaayy! Welcome " name " of " id " t=" (number->string time))]
        [_ (displayln "Unknown message :( I'm scared")])))
  (sleep 1)
  (loop empty))
