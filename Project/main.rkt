#! /usr/bin/env racket
#lang racket

; raco pkg install lens threading libuuid <<< a
(require lens threading
  "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt"
  "network/network.rkt" "poll-buttons.rkt" "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(define-values (id name) (generate-identity))
(info id name)

(define initial-elevator-state (elevator-state id name 0 empty empty empty empty 0 empty))

(let loop ([this-elevator initial-elevator-state]
           [all-elevators (make-immutable-hash)])
  (send this-elevator)
  (sleep 1)
  (let-values ([(this-elevator* all-elevators*) (fold-buttons-into-elevators (pop-button-states) this-elevator all-elevators)])
    ;(set-lights-using-commands (elevator-state-external-requests this-elevator*) (elevator-state-internal-requests this-elevator*))
    ;(let ([messages (filter (lambda (x) (not (string=? (elevator-state-id (first x)) id))) (receive))])
    (let ([messages (receive)])
      (trce messages)
      (trce all-elevators*)
      (if #t
        (~>
          ;; Take messages, make into hash-set of attributes with ttl reset
          (filter-newest-to-hash messages)

          ;; Decrement all 'time-to-live's and discard messages older than we already have
          (prune-old-messages (decrement-time-to-live all-elevators*))

          ;; Update our own current state
          (update-elevator this-elevator*)

          ;; Decrement all 'time-to-live's
          ;; TODO Remove? Handled by decrement-time-to-live now
          ; (map-hash-table (lambda (x)
          ;   (lens-transform elevator-attributes-time-to-live-lens x sub1)))

          ;; Reset 'time-to-live' for the elevators from which we received a message
          ;; TODO Remove? Handled by filter-newest-to-hash
          ; (hash-set-from-list _ elevator-state-id in: messages update-with: (lambda (msg)
          ;   (elevator-attributes msg time-to-live (current-inexact-milliseconds))))

          ;; Refresh time-to-live and timestamp for ourselves
          ;; TODO Remove? Handled by update-elevator
          ; ((lambda (x) (hash-set x id (elevator-attributes-refresh (hash-ref x id)))))

          ;; Remove all elevators where time-to-live <= 0
          (hash-remove-predicate (lambda (x) (<= (elevator-attributes-time-to-live x) 0)))

          (loop this-elevator* _))
        (loop this-elevator* all-elevators*)))))
