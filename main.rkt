#! /usr/bin/env racket
#lang racket

;; This project requires the following packages
;;;; raco pkg install lens libuuid reloadable sha threading

;; This module sets up a reloadable entry point.
;; The reloadable entry point and all its children will
;; automatically be reloaded when their files are updated.

;; The 'reloadable' library used to automatically reload
(require reloadable)

;; Macro to remove 'core' boilerplate
(define-syntax-rule (reloadable name)
  (define name (reloadable-entry-point->procedure (make-reloadable-entry-point 'name (build-path "source" (string-append (symbol->string 'name) ".rkt"))))))

;; Create a reloadable entry point
(reloadable initiator)

;; Main loop of the program
(let loop ([state empty])
  (reload!)
  (loop (initiator state)))
