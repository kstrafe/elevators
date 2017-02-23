#lang racket

(provide invoker)

(require "core-unit.rkt" "data-structures.rkt" "identity-generator-unit.rkt" "logger.rkt" "monitor.rkt" "utilities-unit.rkt")

;; Spawn a monitor that brings this elevator back online in case of failure
(define-values (monitor monitor-out monitor-in monitor-err)
  (subprocess #f #f #f "/usr/bin/env" "racket" "source/monitor.rkt"))
(let ([error (port->string monitor-err)])
  (when (non-empty-string? error) (warn "Unable to spawn monitor" error)))

;; Create a pipe to use for communication between elevator and monitor
(system* "/usr/bin/env" "bash" "mkfifo" "monitor-fifo")

;; Propagate invocation of the main function to core unit
(define (invoker complex-struct)
  ;; Invoke units
  (define-values/invoke-unit/infer identity-generator@)
  (define-values/invoke-unit/infer utilities@)
  (define-values/invoke-unit/infer core@)

  (when (not (empty? complex-struct))
    ; (dbug (first (complex-commands complex-struct)))
    ; (write (first (complex-commands complex-struct)) monitor-pipe-out))
    (with-output-to-file "monitor-fifo"
      (write (first (complex-commands complex-struct)))))

  (if (not (= monitor-out #f))
    (info (read monitor-out))
    empty)

  (core complex-struct))
