#lang racket

(provide invoker)

(require lens "core-unit.rkt" "data-structures.rkt" "identity-generator-unit.rkt" "logger.rkt" "utilities-unit.rkt")

;; Create a pipe to use for communication between elevator and monitor
(when (not (file-exists? "temporaries/monitor-fifo"))
  (system* "/usr/bin/env" "mkfifo" "temporaries/monitor-fifo"))

;; Spawn a monitor that brings this elevator back online in case of failure
(define-values (monitor monitor-out monitor-in monitor-err)
  (subprocess #f #f #f "/usr/bin/env" "setsid" "bash" "-c" "racket source/monitor.rkt"))

;; Open up the fifo pipe
(define monitor-fifo-out (open-output-file "temporaries/monitor-fifo" #:exists 'append))

;; Propagate invocation of the main function to core unit
(define (invoker complex-struct)
  ;; Invoke units
  (define-values/invoke-unit/infer identity-generator@)
  (define-values/invoke-unit/infer utilities@)
  (define-values/invoke-unit/infer core@)

  (when (and (not (empty? complex-struct)) (not (empty? (complex-elevators complex-struct))))
    ; (dbug (first (complex-commands complex-struct)))
    ; (dbug complex-struct)
    (write (lens-view (lens-compose this:command complex-elevators-lens) complex-struct) monitor-fifo-out))

  (flush-output monitor-fifo-out)

  (core complex-struct))
