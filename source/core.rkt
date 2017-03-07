#lang racket

(provide (rename-out (core#io core)))

(require lens threading
         "control-panel.rkt" "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt"
         "lenses.rkt" "logger.rkt" "network.rkt" "utilities.rkt" "utilities-io.rkt")

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(define (core#io complex-struct)
  (if (not (complex? complex-struct))
    (complex (list 0 0) (list empty empty) (list empty empty) empty (spawn-monitor-and-get-fifo))
    (let ([complex* (lens-transform complex-elevators-lens
                                    complex-struct
                                    discuss-good-solution-with-other-elevators-and-execute#io)]
          [cel complex-elevators-lens])
      (when (not (empty? (complex-elevators complex-struct)))
        (let ([message (lens-view cplx:command complex-struct)])
          (write message (complex-monitor-fifo-out complex-struct))))
      (flush-output (complex-monitor-fifo-out complex-struct))

      (~>
        complex*
        (lens-transform cel _ (lambda~>
          (insert-button-presses-into-this-elevator-as-requests (pop-button-states#io) _)
          update-position#io
          store-commands#io
          set-motor-direction-to-task#io))
        move-complex-windows
        (if-changed-call complex-floors    set-floor-indicator#io)
        (if-changed-call complex-calls     set-call-lights#io)
        (if-changed-call complex-commands  set-command-lights#io)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This algorithm consumes a hash-table of elevators and performs side effects with it.
;;
;; It returns a new hash-table of elevators.
(define (discuss-good-solution-with-other-elevators-and-execute#io elevators)
  (if (or (empty? elevators) (not (hash-has-key? elevators id)))
    (discuss-good-solution-with-other-elevators-and-execute#io (hash id (make-empty-elevator#io id name)))
    (begin
      (broadcast#io (lens-view this:state elevators))
      (sleep iteration-sleep-time)
      (let ([current-open (lens-view this:opening elevators)])
        (when (positive? current-open)
          (cond
            ([= current-open door-open-iterations]  (elevator-hardware:open-door#io))
            ([= current-open 1]                     (elevator-hardware:close-door#io))))
        (~>
          (receive#io)
          (filter (lambda~> first state?) _)
          filter-newest-to-hash
          (unify-messages-and-elevators elevators)
          (insert-self-into-elevators elevators)
          decrement-open-door-time
          remove-all-dead-elevators
          decrement-all-time-to-live
          ; trce* ; You can add a trce* anywhere inside a ~> to print the state
          unify-requests
          prune-call-requests-that-are-done
          assign-call-requests
          service-commands
          sort-servicing
          prune-done-requests
          prune-servicing-requests
          detect-and-remove-floor-cycle
          check-for-fatal-situations)))))

;; Moves the window we see the complex struct through.
;;
;; The newst values in the complex are replaced with new values from the current elevator state.
;; The oldest values in the complex are dropped.
;;
;; The newly transformed complex struct is then returned.
(define (move-complex-windows complex)
  (let* ([lt lens-transform] [com (curryr lens-compose complex-elevators-lens)] [get (curryr lens-view complex)])
    (~>
      complex
      (lt complex-floors-lens    _ (lambda (floors)   (list (get (com this:position))  (first floors))))
      (lt complex-calls-lens     _ (lambda (buttons)  (list (get (com this:call))      (first buttons))))
      (lt complex-commands-lens  _ (lambda (buttons)  (list (get (com this:command))   (first buttons)))))))

;; Spawn a monitor that brings this elevator back online in case of failure.
;; Also creates a pipe to use for communication between elevator and monitor.
;;
;; Returns a port referencing the monitor fifo.
(define (spawn-monitor-and-get-fifo)
  (when (not (file-exists? "temporaries/monitor-fifo")) (system* "/usr/bin/env" "mkfifo" "temporaries/monitor-fifo"))
  (subprocess (open-output-file "temporaries/monitor-out" #:exists 'replace) #f 'stdout
              "/usr/bin/env" "setsid" "bash" "-c" "racket source/monitor.rkt")
  (open-output-file "temporaries/monitor-fifo" #:exists 'append))
