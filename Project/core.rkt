#lang racket

(provide core)

;raco pkg install lens threading sha reloadable libuuid
(require lens threading
  "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt"
  "motor.rkt" "network/network.rkt" "poll-buttons.rkt" "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

;; Load/generate an identity
(define-values (id name) (generate-or-load-identity))
(info id name)

;; Retrieve the current elevator state from the hash-table
(define (this-elevator hash) (elevator-attributes-state (hash-ref hash id)))
;; TODO Replace all instances of 'this-elevator' with state-lens, using lenses is cleaner
;; and we can easily modify the state in-place without manually reconstructing elevator-attributes
(define state-lens (lens-compose elevator-attributes-state-lens (hash-ref-lens id)))
(define opening-lens (lens-compose elevator-state-opening-time-lens elevator-attributes-state-lens (hash-ref-lens id)))
(define internal-requests-lens (lens-compose elevator-state-internal-requests-lens state-lens))
(define external-requests-lens (lens-compose elevator-state-external-requests-lens state-lens))
(define position-lens (lens-compose elevator-state-position-lens state-lens))
(define servicing-lens (lens-compose elevator-state-servicing-requests-lens state-lens))

;; The core algorithm consumes a hash-table of elevators
;; and performs side effects with it, returning a new
;; hash-table of elevators.
(define (core elevators)
  ; (trce elevators)
  (if (or (empty? elevators) (not (hash-has-key? elevators id)))
    (core (hash id (make-empty-elevator id name)))
    (begin
      (send (lens-view state-lens elevators))
      (sleep iteration-sleep-time)
      ;; Count down to zero if door open time is non-zero
      ;; We could also sleep manually for a given time and then reset to #f or something
      ;; TODO What do you think?
      (if (> (lens-view opening-lens elevators) 0)
        (begin
          (elevator-hardware-open-door)
          (lens-transform opening-lens elevators sub1))
        (let ([elevators* (fold-buttons-into-elevators (pop-button-states) this-elevator elevators)])
          (set-lights-using-commands (lens-view state-lens elevators*))
          (elevator-hardware-close-door)
          ; (trce (this-elevator elevators))
          (let ([messages (receive)])
            (~>
              ;; Take messages, make into hash-set of attributes with ttl reset
              (filter-newest-to-hash messages)
              ;; Decrement all 'time-to-live's and discard messages older than we already have
              (prune-old-messages (decrement-time-to-live elevators*))
              ;; Update our own current state
              (update-elevator this-elevator)
              ;; Remove all elevators where time-to-live <= 0
              (hash-remove-predicate (lambda (x) (<= (elevator-attributes-time-to-live x) 0)))
              ;; Check if any new floors are reached and use it to change position in the current elevator
              (update-position position-lens)
              ;; Unify external requests and done-requests from all elevators
              unify-requests
              ;; Remove requests from external-requests that are in done-requests
              prune-requests-that-are-done
              ;; Check the current elevator position against the 'servicing requests'
              ;; If it's equal, remove it and set the opening-time value
              (remove-tasks-that-motor-completed this-elevator id)
              ;; Calculate the tasks to work on, to be put into 'servicing requests'
              (compute-the-task-to-take _ this-elevator id)
              ;; Use the 'servicing requests' field to set the motor direction
              (set-motor-direction-to-task! servicing-lens))))))))
