#! /usr/bin/env racket
#lang racket

;; This module provides the following functions
(provide
  elevator-hardware-init
  elevator-hardware-set-motor-direction
  elevator-hardware-set-button-lamp
  elevator-hardware-set-floor-indicator
  elevator-hardware-set-door-open-lamp
  elevator-hardware-set-stop-lamp
  elevator-hardware-get-button-signal
  elevator-hardware-get-floor-sensor-signal
  elevator-hardware-get-stop-signal
  elevator-hardware-get-obstruction-signal

  elevator-hardware-motor-direction
  elevator-hardware-button-type)

(require ffi/unsafe ; We require ffi (foreign function interface) to call C libraries
         (for-syntax racket/list ; Requirements for syntax transformers
                     racket/string
                     racket/syntax
                     syntax/to-string))

;; Load the elevator hardware library
(define driver (ffi-lib (simplify-path (build-path (syntax-source #'here) 'up "libelevator-hardware"))))

;; Define the enumerations used by the library
(define elevator-hardware-motor-direction (_enum '(DIRN_DOWN = -1 DIRN_STOP DIRN_UP)))
(define elevator-hardware-button-type (_enum '(BUTTON_CALL_UP = 0 BUTTON_CALL_DOWN BUTTON_COMMAND)))

;; Macros that generate the foreign function interface
(define-syntax (elevator-hardware syn)
  (let* ([list-of-arguments (syntax->datum syn)]
         [name              (second list-of-arguments)]
         [signature         (third list-of-arguments)]
         [out
    `(define ,(format-symbol "elevator-hardware-~a" name)
      (get-ffi-obj ,(string-append "elev_" (string-replace (symbol->string name) "-" "_"))
        driver ,signature
        (lambda () (error 'ffi-error ,(format "Unable to link to symbol ~a" (symbol->string name))))))])
    (datum->syntax syn out)))

(define-syntax (elevator-hardwares syn)
  (let* ([list-of-arguments (rest (syntax->datum syn))]
         [defines (for/list ((argument list-of-arguments)) `(elevator-hardware ,(first argument) ,(second argument)))])
    (datum->syntax syn `(begin ,@defines))))

;; Create all C bindings
(elevator-hardwares
  (init (_fun -> _void))
  (set-motor-direction (_fun elevator-hardware-motor-direction -> _void))
  (set-button-lamp (_fun elevator-hardware-button-type _int _int -> _void))
  (set-floor-indicator (_fun _int -> _void))
  (set-door-open-lamp (_fun _int -> _void))
  (set-stop-lamp (_fun _int -> _void))
  (get-button-signal (_fun elevator-hardware-button-type _int -> _int))
  (get-floor-sensor-signal (_fun -> _int))
  (get-stop-signal (_fun -> _int))
  (get-obstruction-signal (_fun -> _int)))

(elevator-hardware-init)
