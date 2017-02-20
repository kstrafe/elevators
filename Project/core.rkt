#lang racket

(provide core)

(require lens threading
  "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt"
  "logger.rkt" "motor.rkt" "network.rkt" "poll-buttons.rkt" "utilities.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(struct/lens complex (floors calls commands elevators) #:prefab)

(define (if-changed-call complex accessor procedure)
  (let ([elems (accessor complex)])
    (when (not (equal? (first elems) (second elems)))
      (procedure (first elems))))
  complex)

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
        (if-changed-call complex-floors set-floor-indicator)
        (if-changed-call complex-calls set-call-lights)
        (if-changed-call complex-commands set-command-lights)))))

;; This algorithm consumes a hash-table of elevators
;; and performs side effects with it, returning a new
;; hash-table of elevators.
(define (discuss-good-solution-with-other-elevators-and-execute elevators)
  (if (or (empty? elevators) (not (hash-has-key? elevators id)))
    (discuss-good-solution-with-other-elevators-and-execute
      ; (hash id (make-empty-elevator id name))
#hash(("15e80f40-1e27-4c50-bbbe-da2f3cabd55b"
       .
       #s(attributes
          #s(state
             "15e80f40-1e27-4c50-bbbe-da2f3cabd55b"
             "Gothmog Tempus"
             5
             (#s(request up 7 1487525345355.524) ; 4. Newest
              #s(request up 6 1487525339755.581) ; 2.
              #s(request up 5 1487525339755.581) ; 2.
              #s(request command 3 1487525336224.932) ; 1. Oldest
              #s(request command 0 1487525341443.528)) ; 3.
             (#s(request up 5 1487525339755.581)
              #s(request up 6 1487525339755.581)
              #s(request up 7 1487525345355.524))
             (#s(request command 0 1487525341443.528)
              #s(request command 3 1487525336224.932))
             (#s(request down 4 1487525284307.257)
              #s(request down 2 1487525287041.592)
              #s(request down 1 1487525305713.596)
              #s(request down 2 1487525305713.596)
              #s(request up 3 1487525316618.176)
              #s(request up 4 1487525334334.577))
             0)
          59.0
          1487525627566.098)))
      )
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
              trce* ; You can add a trce* anywhere inside a ~> to print the state
              update-position
              unify-requests
              prune-call-requests-that-are-done
              assign-call-requests
              store-commands
              service-commands
              sort-servicing
              set-motor-direction-to-task!
              prune-done-requests
              prune-servicing-requests
              detect-and-remove-floor-cycle
              check-for-fatal-situations)))))))
