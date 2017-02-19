#lang racket

(provide core)

(require lens threading
  "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt"
  "logger.rkt" "motor.rkt" "network.rkt" "poll-buttons.rkt" "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(struct/lens complex (floors calls commands elevators) #:prefab)

(define (if-floor-change-set-indicator complex)
  (let ([floors (complex-floors complex)])
    (when (not (= (first floors) (second floors)))
      (set-floor-indicator (first floors))))
  complex)

(define (if-calls-change-set-indicator complex)
  (let ([buttons (complex-calls complex)])
    (when (not (equal? (first buttons) (second buttons)))
      (set-call-lights (first buttons))))
  complex)

(define (if-commands-change-set-indicator complex)
  (let ([buttons (complex-commands complex)])
    (when (not (equal? (first buttons) (second buttons)))
      (set-command-lights (first buttons))))
  complex)

(define (core complex-struct)
  (trce complex-struct)
  (if (not (complex? complex-struct))
    (complex (list 0 0) (list empty empty) (list empty empty) empty)
    (let ([complex* (lens-transform complex-elevators-lens complex-struct discuss-good-solution-with-other-elevators)])
        (~>
        (lens-transform complex-floors-lens complex*
          (lambda (floors) (list (lens-view (lens-compose this:position  complex-elevators-lens) complex*) (first floors))))
        (lens-transform complex-calls-lens _
          (lambda (buttons) (list (lens-view (lens-compose this:call     complex-elevators-lens) complex*) (first buttons))))
        (lens-transform complex-commands-lens _
          (lambda (buttons) (list (lens-view (lens-compose this:command  complex-elevators-lens) complex*) (first buttons))))
        if-floor-change-set-indicator
        if-commands-change-set-indicator
        if-calls-change-set-indicator))))

;; The core algorithm consumes a hash-table of elevators
;; and performs side effects with it, returning a new
;; hash-table of elevators.
(define (discuss-good-solution-with-other-elevators elevators)
  (if (or (empty? elevators) (not (hash-has-key? elevators id)))
    (discuss-good-solution-with-other-elevators (hash id (make-empty-elevator id name)))
    (begin
      ; (trce (lens-view this:servicing elevators))
      (broadcast (lens-view this:state elevators))
      (sleep iteration-sleep-time)
      (let ([current-open (lens-view this:opening elevators)])
        (if (positive? current-open)
          (begin
            (cond
              ([= current-open door-open-iterations] (elevator-hardware:open-door))
              ([= current-open 1] (elevator-hardware:close-door)))
            (lens-transform this:opening elevators sub1))
          (let ([elevators* (fold-buttons-into-elevators (pop-button-states) elevators)])
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
              prune-servicing-requests)))))))
