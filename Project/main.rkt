#! /usr/bin/env racket
#lang racket

; raco pkg install lens threading libuuid <<< a
(require lens threading "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt" "network/network.rkt" "poll-buttons.rkt" "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(struct/lens elevator-state (id name position servicing-requests external-requests internal-requests done-requests resting-position opening-time) #:prefab)
(struct/lens elevator-attributes (state time-to-live timestamp) #:prefab)

(define (elevator-attributes-refresh attributes)
  (elevator-attributes (elevator-attributes-state attributes) time-to-live (current-inexact-milliseconds)))

(define-values (id name) (generate-identity))

(define initial-elevator-state (elevator-state id name 0 empty empty empty empty 0 empty))
(define time-to-live 3)

(info id name)

(let loop ([this-elevator initial-elevator-state]
           [all-elevators (make-immutable-hash)])
  (send this-elevator)
  (sleep 1)
  (let* ([button-presses (pop-button-states)]
         [commands (filter (lambda (x) (eq? (first x) 'command)) button-presses)]
         [this-elevator* (foldl (lambda (c s) (lens-transform elevator-state-internal-requests-lens s (lambda (x) (cons c x)))) this-elevator commands)]
         [non-commands (filter (lambda (x) (not (eq? (first x) 'command))) button-presses)]
         [this-elevator** (foldl (lambda (c s) (lens-transform elevator-state-external-requests-lens s (lambda (x) (cons c x)))) this-elevator* non-commands)]
         [all-elevators* (hash-set all-elevators (elevator-state-id this-elevator) (elevator-attributes this-elevator time-to-live (current-inexact-milliseconds)))])
    (let ([messages (map first (receive))])
      (trce messages)
      (~>
        ;; Decrement all 'time-to-live's
        (map-hash-table all-elevators* (lambda (x)
          (lens-transform elevator-attributes-time-to-live-lens x sub1)))

        ;; Reset 'time-to-live' for the elevators from which we received a message
        (hash-set-from-list _ elevator-state-id in: messages update-with: (lambda (msg)
          (elevator-attributes msg time-to-live (current-inexact-milliseconds))))

        ;; Refresh time-to-live and timestamp for ourselves
        ((lambda (x) (hash-set x id (elevator-attributes-refresh (hash-ref x id)))))

        ;; Remove all elevators where time-to-live <= 0
        (hash-remove-predicate (lambda (x) (<= (elevator-attributes-time-to-live x) 0)))

        (loop this-elevator** _)))))
