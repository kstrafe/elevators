#! /usr/bin/env racket
#lang racket

; raco pkg install lens threading libuuid <<< a
(require lens
         threading
         "elevator-hardware/elevator-interface.rkt"
         "identity-generator.rkt"
         "network/network.rkt"
         "poll-buttons.rkt"
         "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(struct/lens elevator-state (id name position servicing-requests external-requests internal-requests resting-position opening-time) #:prefab)
(struct/lens elevator-attributes (state time-to-live timestamp) #:prefab)

(define (elevator-attributes-refresh attributes)
  (elevator-attributes (elevator-attributes-state attributes) time-to-live (current-inexact-milliseconds)))

(define-values (id name) (generate-identity))

(define state (elevator-state id name 0 empty empty empty 0 empty))
(define time-to-live 3)

(info id name)

(let loop ([my-friends (make-immutable-hash `([,id . ,(elevator-attributes state time-to-live (current-inexact-milliseconds))]))])
  (send state)
  (sleep 1)
  (let* ([button-presses (pop-button-states)]
         [commands (filter (lambda (x) (eq? (first x) 'command)) button-presses)]
         [ins (foldl (lambda (c s) (lens-transform elevator-state-internal-requests-lens s (lambda (x) (cons c x)))) state commands)])
    (dbug ins))
  (let ([messages (receive)])
    ; (trce my-friends)
    (~>
      ;; Decrement all 'time-to-live's
      (map-hash-table my-friends (lambda (x)
        (lens-transform elevator-attributes-time-to-live-lens x sub1)))

      ;; Reset 'time-to-live' for the elevators from which we received a message
      (hash-set-from-list _ elevator-state-id in: messages update-with: (lambda (msg)
        (elevator-attributes msg time-to-live (current-inexact-milliseconds))))

      ;; Refresh time-to-live and timestamp for ourselves
      ((lambda (x) (hash-set x id (elevator-attributes-refresh (hash-ref x id)))))

      ;; Remove all elevators where time-to-live <= 0
      (hash-remove-predicate (lambda (x) (<= (elevator-attributes-time-to-live x) 0)))

      (loop))))
