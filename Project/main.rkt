#! /usr/bin/env racket
#lang racket

(require libuuid
         "name-generator.rkt")

(define (generate-identity)
  (let ([filepath "_elevator-uuid"])
    (if (file-exists? filepath)
      (with-input-from-file filepath
        (lambda () (apply values (file->lines filepath))))
      (let ([id (uuid-generate)]
            [name (generate-name)])
        (with-output-to-file filepath
          (lambda () (printf (string-join (list id name) "\n"))))
        (values id name)))))

(define-values (id name) (generate-identity))

;; DEBUG
id
name