#! /usr/bin/env racket
#lang racket

(require libuuid
         "name-generator.rkt")

(define (generate-identity)
  (let ([filepath "_elevator-uuid"])
    (if (file-exists? filepath)
      (let* ([in (open-input-file filepath)]
             [id (read-line in 'any)]
             [name (read-line in 'any)])
        (close-input-port in)
        (values id name))
      (let* ([id (uuid-generate)]
             [name (generate-name)]
             [out (open-output-file filepath)])
        (write-string id out)
        (newline out)
        (write-string name out)
        (newline out)
        (close-output-port out)
        (values id name)))))

(define-values (id name) (generate-identity))

;; DEBUG
id
name
