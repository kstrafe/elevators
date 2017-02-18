#lang racket

(provide core)

; raco pkg install lens threading sha reloadable libuuid
(require lens threading
  "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt"
  "logger.rkt" "motor.rkt" "network/network.rkt" "poll-buttons.rkt" "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

;; The core algorithm consumes a hash-table of elevators
;; and performs side effects with it, returning a new
;; hash-table of elevators.
(define (core elevators)
  (if (or (empty? elevators) (not (hash-has-key? elevators id)))
    (core (hash id (make-empty-elevator id name)))
    (begin
      (send (lens-view state-lens elevators))
      (sleep iteration-sleep-time)
      ; (trce elevators)
      ;; Count down to zero if door open time is non-zero
      ;; We could also sleep manually for a given time and then reset to #f or something
      ;; TODO What do you think?
      (if (> (lens-view opening-lens elevators) 0)
        (begin
          (elevator-hardware-open-door)
          (lens-transform opening-lens elevators sub1))
        (let ([elevators* (fold-buttons-into-elevators (pop-button-states) elevators)])
          ; (dbug elevators*)
          (set-lights-using-commands (lens-view state-lens elevators*))
          (elevator-hardware-close-door)
          ; (trce elevators*)
          (~>
            ;; Receive all messages sent by all elevators
            (receive)
            ;; Take messages, make into hash-table of attributes
            filter-newest-to-hash
            ;; Discard messages older than we already have
            (unify-messages-and-elevators elevators*)
            ;; Because we may not get a message from ourselves, we need to manually insert ourselves
            (insert-self-into-elevators elevators*)
            ;; Remove all elevators where time-to-live <= 0
            remove-dead-elevators
            ;; Decrement ttl. We do this _after_ remove-dead-elevators
            decrement-time-to-live
            ;; Check if any new floors are reached and use it to change position in the current elevator
            (update-position position-lens)
            ;; Unify external requests and done-requests from all elevators
            unify-requests
            ;; Remove requests from external-requests that are in done-requests
            prune-external-requests-that-are-done
            ;; Calculate the tasks to work on, to be put into 'servicing requests'
            try-self-assign-external-task
            (sort-servicing servicing-lens state-lens)
            ;; Use the 'servicing requests' field to set the motor direction
            (set-motor-direction-to-task! servicing-lens)
            ;; Check the current elevator position against the 'servicing requests'
            ;; If it's equal, remove it and set the opening-time value
            (prune-servicing-requests servicing-lens done-lens opening-lens position-lens internal-requests-lens)))))))
