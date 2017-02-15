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

(let loop ([this-elevator (elevator-state id name 0 empty empty empty empty 0 empty)]
           [all-elevators (make-immutable-hash)])
  (send this-elevator)
  (sleep iteration-sleep-time)
  (let-values ([(this-elevator* all-elevators*) (fold-buttons-into-elevators (pop-button-states) this-elevator all-elevators)])
    (set-lights-using-commands (elevator-state-external-requests this-elevator*) (elevator-state-internal-requests this-elevator*))
    (let ([messages (receive)])
      (trce all-elevators*)
      (~>
        ;; Take messages, make into hash-set of attributes with ttl reset
        (filter-newest-to-hash messages)

        ;; Decrement all 'time-to-live's and discard messages older than we already have
        (prune-old-messages (decrement-time-to-live all-elevators*))

        ;; Update our own current state
        (update-elevator this-elevator*)

        ;; Remove all elevators where time-to-live <= 0
        (hash-remove-predicate (lambda (x) (<= (elevator-attributes-time-to-live x) 0)))
        unify-requests
        prune-requests-that-are-done
        (loop this-elevator* _)))))
