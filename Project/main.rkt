#! /usr/bin/env racket
#lang racket

(require libuuid
         "name-generator.rkt")

(define (write-identity filepath)
  (let ([id (uuid-generate)]
        [name (generate-name)])
    (with-output-to-file filepath
      (lambda () (write (list id name) )))
    (values id name)))

(define (generate-identity)
  (let ([filepath "_elevator-uuid"])
    (if (file-exists? filepath)
      (with-handlers ([exn:fail:filesystem?
                        (lambda (e)
                          (displayln "file not readable")
                          (delete-file filepath)
                          (write-identity filepath))])
        (with-input-from-file filepath
          (lambda () (apply values (file->value filepath)))))
      (write-identity filepath))))

(define-values (id name) (generate-identity))

;; DEBUG
id
name
