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

;; The core algorithm consumes a hash-table of elevators
;; and performs side effects with it, returning a new
;; hash-table of elevators.
(define (core elevators)
  ; (set-motor-to 3)
  ; (trce elevators)
  (if (or (empty? elevators) (not (hash-has-key? elevators id)))
    (core (hash id (make-empty-elevator id name)))
    (begin
      (dbug (lens-view state-lens elevators))
      (send (this-elevator elevators))
      (sleep iteration-sleep-time)
      (let ([elevators* (fold-buttons-into-elevators (pop-button-states) this-elevator elevators)])
        (set-lights-using-commands (this-elevator elevators*))
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
            ;; Unify external requests and done-requests into
            unify-requests
            prune-requests-that-are-done
            (compute-the-task-to-take _ this-elevator id)
            (set-motor-direction-to-task this-elevator)
            (remove-tasks-that-motor-completed this-elevator id)))))))
