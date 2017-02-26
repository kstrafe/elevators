#lang racket

(provide (all-defined-out))

(require lens threading "data-structures.rkt" "lenses.rkt" "logger.rkt" "motor.rkt")

;; Read backup commands from current command line arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPURE : This procedure is impure as it reads from command line arguments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (read-backup-commands#io)
  (~>
    (vector-ref (current-command-line-arguments) 0)
    open-input-string
    read))

;; Read commands from file
;; If no file is found or is corrupted, try to read from a backup fifo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPURE : This procedure is impure as it reads from a file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (read-commands#io)
  (with-handlers ([exn? (lambda (error) (warn error))])
    (let* ([filepath "temporaries/commands"]
           [result (if (file-exists? filepath) (file->value filepath) (read-backup-commands#io))])
      (if (eof-object? result) (read-backup-commands#io) result))))

;; Create a new empty elevator
(define (make-empty-elevator#io id name)
  (attributes (state id name 0 empty empty (read-commands#io) empty 0) time-to-live (current-inexact-milliseconds)))

;; Update the position number in the current elevator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPURE : This procedure is impure as it reads a value coming from the detectors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-position#io elevators)
  (if (is-blocked?#io)
    (begin
      (crit "Motor is blocked")
      (sleep 1)
      (update-position#io elevators))
    (let ([floor (any-new-floor-reached?#io)])
      (if floor
        (lens-set this:position elevators floor)
        elevators))))

;; Store commands locally, to be restored in case of power loss
;;
;; Writes the commands to a file. In case of any power loss, the elevator
;; should read the file to restore all commands that were queued.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPURE : This procedure is impure as it writes to a file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (store-commands#io elevators)
  (with-handlers ([exn? (lambda (error) (crit error))])
    (with-output-to-file "temporaries/commands" (lambda () (pretty-write (lens-view this:command elevators))) #:exists 'replace))
  elevators)

;; Calls move-to-floor depending on the top of the 'servicing-requests'
;; field in the current elevator.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPURE : This procedure is impure as it sets the direction of the elevator motor ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (set-motor-direction-to-task#io elevators)
  (when (not (positive? (lens-view this:opening elevators)))
    (let ([servicing-requests (lens-view this:servicing elevators)])
      (when (not (empty? servicing-requests))
        (~>
          (first servicing-requests)
          request-floor
          move-to-floor#io))))
  elevators)

