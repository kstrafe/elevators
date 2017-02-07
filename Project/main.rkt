#! /usr/bin/env racket
#lang racket

(require "identity-generator.rkt")

(define-values (id name) (generate-identity))

;; DEBUG
id
name
