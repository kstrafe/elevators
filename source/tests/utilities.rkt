#lang racket

(require rackunit rackunit/text-ui "../utilities.rkt")

(define-syntax-rule (check-equals? call ((input ...) result) ...)
  (begin (check-equal? (call input ...) result) ...))

(define-syntax-rule (check-equals/list? call ((input ...) result) ...)
  (begin (check-equal? (call (quote (input ...))) result) ...))

(check-equals/list? first-or-empty
  (()    empty)
  ((1)   1)
  ((2 2) 2)
  ((1 2) 1)
  ((2 1) 2)
  ((a b) 'a)
  ((b a) 'b))

(check-equal? (map-hash-table #hash((a . 0) (10 . 11)) add1)    #hash((a . 1) (10 . 12)))
(check-equal? (map-hash-table #hash((a . "0")) string->number)  #hash((a . 0)))

(check-equal? (hash-remove-predicate #hash((a . 0) (b . "text")) number?)  #hash((b . "text")))
(check-equal? (hash-remove-predicate #hash((a . 0) (b . "text") (c . "other string")) string?)  #hash((a . 0)))

(check-equal? (same-request? #s(request up 0 0) #s(request up 0 1)) #t)
(check-equal? (same-request? #s(request up 0 0) #s(request up 1 0)) #f)
(check-equal? (same-request? #s(request up 0 0) #s(request up 1 1)) #f)
(check-equal? (same-request? #s(request up 1 0) #s(request up 1 1)) #t)

(define prune-requests-tests
  (test-suite "Test prune-requests"
    (check-equals? prune-requests
      (('() '())                                                                  '())
      (('(#s(request up 0 0)) '(#s(request up 0 0)))            '(#s(request up 0 0)))
      (('(#s(request up 1 0)) '(#s(request up 1 0)))            '(#s(request up 1 0)))
      (('(#s(request up 1 2)) '(#s(request up 1 0)))            '(#s(request up 1 0)))
      (('(#s(request up 1 0)) '(#s(request up 1 2)))            '(#s(request up 1 2)))

      (('(#s(request down 0 0)) '(#s(request up 0 0)))          '(#s(request down 0 0) #s(request up 0 0)))
      (('(#s(request up 0 0)) '(#s(request down 0 0)))          '(#s(request up 0 0) #s(request down 0 0)))
      (('(#s(request down 0 1)) '(#s(request down 0 0)))        '(#s(request down 0 0)))

      (((for/list ([i (range 50 150)]) `#s(request command ,i 0)) (for/list ([i 100]) `#s(request command ,i 0)))
        (append (for/list ([i (range 100 150)]) `#s(request command ,i 0)) (for/list ([i 100]) `#s(request command ,i 0)))))))

(run-tests prune-requests-tests)
