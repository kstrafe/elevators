#! /usr/bin/env racket
#lang racket

;; This project requires the following packages
;;;; raco pkg install lens libuuid reloadable sha threading

;; This module sets up a reloadable entry point.
;; The reloadable entry point and all its children will
;; automatically be reloaded when their files are updated.

;; The 'reloadable' library used to automatically reload
(require reloadable "source/data-structures.rkt" "source/logger.rkt" "source/monitor.rkt")

;; Macro to remove 'core' boilerplate
(define-syntax-rule (reloadable name)
  (define name (reloadable-entry-point->procedure (make-reloadable-entry-point 'name (build-path "source" (string-append (symbol->string 'name) ".rkt"))))))

;; Spawn a monitor that brings this elevator back online in case of failure
(define-values (monitor _ monitor-in monitor-err)
  (subprocess (current-output-port) #f 'stdout "/usr/bin/env" "racket" "source/monitor.rkt"))
; (let ([error (port->string monitor-err)]) (when (non-empty-string? error) (warn "Unable to spawn monitor" error)))

;; Create a pipe to use for communication between elevator and monitor
; (define-values (monitor-pipe-in monitor-pipe-out) (make-pipe))

;; Create a reloadable entry point
(reloadable invoker)

;; Main loop of the program
(let loop ([state empty])
  (reload!)
  (when (not (empty? state))
    ; (dbug (first (complex-commands state)))
    ; (write (first (complex-commands state)) monitor-pipe-out))
    (write (first (complex-commands state)) monitor-in))
  (loop (invoker state)))
