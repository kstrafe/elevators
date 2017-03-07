#lang racket

(provide (all-defined-out))

(require lens threading "data-structures.rkt" "lenses.rkt" "logger.rkt" "motor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                        ;;
;; IMPURE MODULE                                          ;;
;;                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                        ;;
;; This module contains impure functions.                 ;;
;;                                                        ;;
;; Functions annotated with "#io" at the end of their     ;;
;; names are impure because they make use of input/output ;;
;; operations.                                            ;;
;;                                                        ;;
;; Functions annotated with "!" at the end of their name  ;;
;; is impure because they perform side-effects/mutation.  ;;
;;                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a new empty elevator.
;;
;; Try to restore commands if it's possible.
(define (make-empty-elevator#io id name)
  (attributes (state id name 0 empty empty (read-commands#io) empty 0) time-to-live (current-inexact-milliseconds)))

;; Read backup commands from current command line arguments
(define (read-backup-commands#io)
  (let ([args (current-command-line-arguments)])
    (if (or (void? args) (= (vector-length args) 0))
      empty
      (~>
        (vector-ref args 0)
        open-input-string
        read))))

;; Calls move-to-floor depending on the top of the 'servicing-requests' field in the current elevator
(define (set-motor-direction-to-task#io elevators)
  (when (not (positive? (lens-view this:opening elevators)))
    (let ([servicing-requests (lens-view this:servicing elevators)])
      (when (not (empty? servicing-requests))
        (~>
          (first servicing-requests)
          request-floor
          move-to-floor#io))))
  elevators)

;; Store commands locally, to be restored in case of power loss.
;;
;; Writes the commands to a file.
;; In case of any power loss, the elevator should read the file to restore all commands that were queued.
(define (store-commands#io elevators)
  (with-handlers ([exn? (lambda (error) (crit error))])
    (with-output-to-file "temporaries/commands"
                         (lambda () (pretty-write (lens-view this:command elevators)))
                         #:exists 'replace))
  elevators)

;; Read commands from file.
;;
;; If no file is found or is corrupted, try to read from a backup fifo.
(define (read-commands#io)
  (with-handlers ([exn? (lambda (error) (warn error))])
    (let* ([filepath "temporaries/commands"]
           [result (if (file-exists? filepath) (file->value filepath) (read-backup-commands#io))])
      (if (eof-object? result) (read-backup-commands#io) result))))

;; Update the position number in the current elevator
(define (update-position#io elevators)
  (if (is-blocked?#io)
    (begin (crit "Motor is blocked") (sleep 1) (update-position#io elevators))
    (let ([floor (any-new-floor-reached?#io)])
      (if floor (lens-set this:position elevators floor) elevators))))
