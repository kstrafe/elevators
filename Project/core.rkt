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
      (trce (lens-view this:servicing elevators))
      (broadcast (lens-view this:state elevators))
      (sleep iteration-sleep-time)
      (set-floor-indicator-using-elevator (lens-view this:state elevators))
      (if (> (lens-view this:opening elevators) 0)
        (begin
          (elevator-hardware:open-door)
          (lens-transform this:opening elevators sub1))
        (let ([elevators* (fold-buttons-into-elevators (pop-button-states) elevators)])
          (set-lights-using-commands (lens-view this:state elevators*))
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
            update-position
            unify-requests
            prune-call-requests-that-are-done
            assign-call-requests
            sort-servicing
            set-motor-direction-to-task!
            prune-done-requests
            prune-servicing-requests))))))
