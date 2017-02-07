#! /usr/bin/env racket
#lang racket

(require "identity-generator.rkt"
         "utilities.rkt")

(define-values (id name) (generate-identity))

(dbug id name)
