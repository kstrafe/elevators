#! /usr/bin/env racket
#lang racket

(require libuuid
         "name-generator.rkt")

(define-values (id name) (values (uuid-generate) (generate-name)))

;; DEBUG
id
name
