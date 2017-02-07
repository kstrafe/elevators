#! /usr/bin/env racket
#lang racket

(require "identity-generator.rkt"
         "network/network.rkt")

(define-values (id name) (generate-identity))

(let loop ()
  (send (string-append "Hi! My name is " name ", I'm also known as " id))
  (displayln (receive))
  (sleep 2)
  (loop))
