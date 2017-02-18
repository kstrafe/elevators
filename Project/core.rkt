#lang racket

(provide core)

(require lens threading
  "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt"
  "logger.rkt" "motor.rkt" "network.rkt" "poll-buttons.rkt" "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

;; The core algorithm consumes a hash-table of elevators
;; and performs side effects with it, returning a new
;; hash-table of elevators.
(define (core elevators)
  (if (or (empty? elevators) (not (hash-has-key? elevators id)))
    (core (hash id (make-empty-elevator id name)))
    (begin
      (broadcast (lens-view state-lens elevators))
      (sleep iteration-sleep-time)
      (set-floor-indicator-using-elevator (lens-view state-lens elevators))
      (if (> (lens-view opening-lens elevators) 0)
        (begin
          (elevator-hardware:open-door)
          (lens-transform opening-lens elevators sub1))
        (let ([elevators* (fold-buttons-into-elevators (pop-button-states) elevators)])
          (set-lights-using-commands (lens-view state-lens elevators*))
          (elevator-hardware:close-door)
          ; (trce elevators*)
          (~>
            (receive)
            filter-newest-to-hash
            (unify-messages-and-elevators elevators*)
            (insert-self-into-elevators elevators*)
            remove-dead-elevators
            decrement-time-to-live
            ; trce* ; You can add a trce* anywhere inside a ~> to print the state
            (update-position position-lens)
            unify-requests
            prune-call-requests-that-are-done
            try-self-assign-external-task
            (sort-servicing servicing-lens state-lens)
            ; trce*
            (set-motor-direction-to-task! servicing-lens)
            (prune-servicing-requests servicing-lens done-lens opening-lens position-lens command-requests-lens)))))))
