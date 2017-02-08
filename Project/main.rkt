#! /usr/bin/env racket
#lang racket

(require lens
         threading
         "identity-generator.rkt"
         "network/network.rkt"
         "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(struct/lens elevator-state (id name position) #:prefab)
(struct/lens elevator-attributes (state time-to-live timestamp) #:prefab)

(define-values (id name) (generate-identity))

(define state (elevator-state id name 0))
(define time-to-live 3)

(dbug id name)

;; Map each value in a hash-table (hash fn . -> . hash)
(define (map-hash-table hash function)
  (foldl (lambda (x s) (hash-set s x (function (hash-ref s x)))) hash (hash-keys hash)))

(define-syntax hash-set-from-list
  (syntax-rules (in: update-with:)
    [(_ hash accessor in: list update-with: function)
      (foldl (lambda (x s) (hash-set s (accessor x) (function x)))
        hash
        list)]))

(define (hash-remove-predicate hash predicate)
  (foldl (lambda (x s)
    (if (predicate (hash-ref s x))
      (hash-remove s x)
      s))
    hash
    (hash-keys hash)))

(let loop ([my-friends (make-immutable-hash `([,id . ,(elevator-attributes state time-to-live (current-inexact-milliseconds))]))])
  (send state)
  (sleep 1)
  (let ([messages (receive)])
    ; (trce messages)
    (trce my-friends)
    (~>
      ;; Decrement all 'time-to-live's
      (map-hash-table my-friends (lambda (x)
        (lens-transform elevator-attributes-time-to-live-lens x sub1)))

      ;; Reset 'time-to-live' for the elevators from which we received a message
      (hash-set-from-list _ elevator-state-id in: messages update-with: (lambda (msg)
        (elevator-attributes msg time-to-live (current-inexact-milliseconds))))

      ;; Remove all elevators where time-to-live <= 0
      (hash-remove-predicate (lambda (x) (<= (elevator-attributes-time-to-live x) 0)))

      (loop))))
