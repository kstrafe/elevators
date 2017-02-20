#lang racket

(provide generate-or-load-identity)

(require libuuid)

(define names (file->value "names"))

(define (select-random-name)
  (let ([len (length names)])
    (list-ref names (random len))))

(define (generate-name)
  (string-join (list (select-random-name) (select-random-name))))

(define (generate-or-load-identity)
  (with-handlers ([exn?
    (lambda (e)
      (displayln e)
      (values (uuid-generate) (generate-name)))])
    (let ([filepath "identity"])
      (if (file-exists? filepath)
          (apply values (file->value filepath))
        (let ([id (uuid-generate)]
              [name (generate-name)])
          (with-output-to-file filepath
            (lambda () (write (list id name))))
          (values id name))))))
