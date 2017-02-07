#! /usr/bin/env racket
#lang racket

(require libuuid
         "name-generator.rkt")

(define (generate-identity)
  (with-handlers ([exn:fail:filesystem?
                    (lambda (e)
                      (displayln "filesystem error")
                      (values (uuid-generate) (generate-name)))]
                  [exn:fail:read?
                    (lambda (e)
                      (displayln "file to value error")
                      (values (uuid-generate) (generate-name)))])
    (let ([filepath "_elevator-uuid"])
      (if (file-exists? filepath)
        (with-input-from-file filepath
          (lambda () (apply values (file->value filepath))))
        (let ([id (uuid-generate)]
              [name (generate-name)])
          (with-output-to-file filepath
            (lambda () (write (list id name))))
          (values id name))))))

(define-values (id name) (generate-identity))

;; DEBUG
id
name
