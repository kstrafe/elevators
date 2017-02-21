#lang racket/unit

(require lens racket/list threading
  "invoker-sig.rkt" "core-sig.rkt" "utilities-sig.rkt"
  "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt"
  "logger.rkt" "motor.rkt" "network.rkt" "control-panel.rkt")

(import invoker^ utilities^)
(export core^)

(info "Core started")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(define (core complex-struct)
  ; (trce complex-struct)
  (if (not (complex? complex-struct))
    (complex (list 0 0) (list empty empty) (list empty empty) empty)
    (let ([complex* (lens-transform complex-elevators-lens complex-struct discuss-good-solution-with-other-elevators-and-execute)])
      (~>
        (lens-transform complex-floors-lens complex*
          (lambda (floors) (list (lens-view (lens-compose this:position  complex-elevators-lens) complex*) (first floors))))
        (lens-transform complex-calls-lens _
          (lambda (buttons) (list (lens-view (lens-compose this:call     complex-elevators-lens) complex*) (first buttons))))
        (lens-transform complex-commands-lens _
          (lambda (buttons) (list (lens-view (lens-compose this:command  complex-elevators-lens) complex*) (first buttons))))
        (if-changed-call complex-floors set-floor-indicator#io)
        (if-changed-call complex-calls set-call-lights#io)
        (if-changed-call complex-commands set-command-lights#io)))))

;; This algorithm consumes a hash-table of elevators
;; and performs side effects with it, returning a new
;; hash-table of elevators.
(define (discuss-good-solution-with-other-elevators-and-execute elevators)
  (if (or (empty? elevators) (not (hash-has-key? elevators id)))
    (discuss-good-solution-with-other-elevators-and-execute (hash id (make-empty-elevator id name)))
    (begin
      ; (trce (lens-view this:servicing elevators))
      (broadcast#io (lens-view this:state elevators))
      (sleep iteration-sleep-time)
      (let ([current-open (lens-view this:opening elevators)])
        (if (positive? current-open)
          (begin
            (cond
              ([= current-open door-open-iterations] (elevator-hardware:open-door#io))
              ([= current-open 1] (elevator-hardware:close-door#io)))
            (lens-transform this:opening elevators sub1))
          (let ([elevators* (insert-button-presses-into-this-elevator-as-requests (pop-button-states#io) elevators)])
            (~>
              (receive#io)
              filter-newest-to-hash
              (unify-messages-and-elevators elevators*)
              (insert-self-into-elevators elevators*)
              remove-all-dead-elevators
              decrement-all-time-to-live
              ; trce* ; You can add a trce* anywhere inside a ~> to print the state
              update-position#io
              unify-requests
              prune-call-requests-that-are-done
              assign-call-requests
              store-commands#io
              service-commands
              sort-servicing
              set-motor-direction-to-task#io
              prune-done-requests
              prune-servicing-requests
              detect-and-remove-floor-cycle
              check-for-fatal-situations)))))))
