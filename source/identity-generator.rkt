#lang racket

(provide generate-or-load-identity#io)

(require libuuid)

(define names (file->value "resources/names"))

(define (select-random-name#io)
  (let ([len (length names)])
    (list-ref names (random len))))

(define (generate-name#io)
  (string-join (list (select-random-name#io) (select-random-name#io))))

(define (generate-or-load-identity#io)
  (with-handlers ([exn?
    (lambda (e)
      (displayln e)
      (values (uuid-generate) (generate-name#io)))])
    (let ([filepath "temporaries/identity"])
      (if (file-exists? filepath)
          (apply values (file->value filepath))
        (let ([id (uuid-generate)]
              [name (generate-name#io)])
          (with-output-to-file filepath
            (lambda () (write (list id name))))
          (values id name))))))
