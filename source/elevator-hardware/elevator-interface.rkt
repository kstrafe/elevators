#lang racket

(provide (prefix-out elevator-hardware: (all-defined-out)))

(require (for-syntax racket/list racket/string racket/syntax syntax/to-string) ; Requirements for syntax transformers
         ffi/unsafe ; We require ffi (foreign function interface) to call C libraries
         racket/runtime-path "../logger.rkt")

;; Controls whether the hardware should look for a real or simulated elevator
(define simulated #f)

(define (open-door#io)  (set-door-open-lamp#io 1))
(define (close-door#io) (set-door-open-lamp#io 0))

;; Load the elevator hardware library
(define-runtime-path library "libelevator-hardware")
(define-runtime-path library-file "libelevator-hardware.so")
(define-runtime-path this-folder ".")
(when (not (file-exists? library-file))
  (let-values ([(program out in err)
    (subprocess #f #f #f "/usr/bin/env" "bash" "-c" (string-append "cd " (path->string this-folder) " && make"))])
    (let ([error (port->string err)])
      (when (non-empty-string? error)
        (ftal "Unable to build interface" error)
        (exit 1)))
    (port->string out)))
(define driver (ffi-lib library))

;; Define the enumerations used by the library
(define type             (_enum '(ET_Comedi ET_Simulation)))
(define motor-direction  (_enum '(DIRN_DOWN = -1 DIRN_STOP DIRN_UP)))
(define button-list     '(BUTTON_CALL_UP BUTTON_CALL_DOWN BUTTON_COMMAND))
(define button-type      (_enum button-list))

;; Macros that generate the foreign function interface
(define-syntax (elevator-hardware syn)
  (let* ([list-of-arguments (syntax->datum syn)]
         [name              (second list-of-arguments)]
         [signature         (third list-of-arguments)]
         [out
    `(define ,(format-symbol "~a#io" name)
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
  (init (_fun type -> _void))
  (set-motor-direction (_fun motor-direction -> _void))
  (set-button-lamp (_fun button-type _int _int -> _void))
  (set-floor-indicator (_fun _int -> _void))
  (set-door-open-lamp (_fun _int -> _void))
  (set-stop-lamp (_fun _int -> _void))
  (get-button-signal (_fun button-type _int -> _int))
  (get-floor-sensor-signal (_fun -> _int))
  (get-stop-signal (_fun -> _int))
  (get-obstruction-signal (_fun -> _int)))

(init#io (if simulated 'ET_Simulation 'ET_Comedi))
