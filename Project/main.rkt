#! /usr/bin/env racket
#lang racket

; raco pkg install lens threading libuuid <<< a
(require lens threading
  "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt"
  "network/network.rkt" "poll-buttons.rkt" "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(define (elevator-attributes-refresh attributes)
  (elevator-attributes (elevator-attributes-state attributes) time-to-live (current-inexact-milliseconds)))

(define-values (id name) (generate-identity))
(info id name)

(define initial-elevator-state (elevator-state id name 0 empty empty empty empty 0 empty))

(let loop ([this-elevator initial-elevator-state]
           [all-elevators (make-immutable-hash)])
  (send this-elevator)
  (sleep 1)
  (let-values ([(this-elevator* all-elevators*) (fold-buttons-into-elevators (pop-button-states) this-elevator all-elevators)])
    (set-lights-using-commands (elevator-state-external-requests this-elevator*) (elevator-state-internal-requests this-elevator*))
    (let ([messages (filter (lambda (x) (not (string=? (elevator-state-id (first x)) id))) (receive))])
      ;; TODO For each ID, time needs to be compared to each other AND all-elevators* time
      (if #f
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

          (loop this-elevator* _))
        (loop this-elevator* all-elevators*)))))
