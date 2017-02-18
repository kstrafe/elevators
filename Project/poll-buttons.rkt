#lang racket

(provide pop-button-states set-floor-indicator-using-elevator set-lights-using-commands)

(require lens racket/async-channel "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "logger.rkt")

;; Get all button presses. Remove duplicates. Add timestamps.
(define (pop-button-states)
  (map (curry set-command-timestamp (current-inexact-milliseconds))
    (remove-duplicates
      (let loop ()
        (let ([button (async-channel-try-get button-channel)])
          (if button
            (cons (buttonify button) (loop))
            empty))))))

;; Sets the floor indicator based on this elevator's position
(define (set-floor-indicator-using-elevator elevator)
  (elevator-hardware-set-floor-indicator (elevator-state-position elevator)))

;; Set the current elevator's lights using the button states
(define (set-lights-using-commands elevator)
  (let* ([ext-cmds       (elevator-state-external-requests elevator)]
         [ext-cmds-up    (map external-command-floor (filter (lambda (x) (symbol=? (external-command-direction x) 'up)) ext-cmds))]
         [ext-cmds-down  (map external-command-floor (filter (lambda (x) (symbol=? (external-command-direction x) 'down)) ext-cmds))]
         [int-cmds       (map internal-command-floor (elevator-state-internal-requests elevator))])
    (for ([floor (range floor-count)])
      (elevator-hardware-set-button-lamp 'BUTTON_COMMAND   floor (if (ormap (curry = floor) int-cmds) 1 0))
      (elevator-hardware-set-button-lamp 'BUTTON_CALL_UP   floor (if (ormap (curry = floor) ext-cmds-up) 1 0))
      (elevator-hardware-set-button-lamp 'BUTTON_CALL_DOWN floor (if (ormap (curry = floor) ext-cmds-down) 1 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define button-channel (make-async-channel))

(define (set-command-timestamp time command)
  (if (internal-command? command)
    (lens-set internal-command-timestamp-lens command time)
    (lens-set external-command-timestamp-lens command time)))

(define (buttonify type)
  (match type
    [(list 'BUTTON_CALL_UP   floor)  (external-command 'up   floor 0)]
    [(list 'BUTTON_CALL_DOWN floor)  (external-command 'down floor 0)]
    [(list 'BUTTON_COMMAND   floor)  (internal-command       floor 0)]
    [_ type]))

(define (set-and-send type state floor)
  (when (= state 1)
    (elevator-hardware-set-button-lamp type floor state)
    (async-channel-put button-channel (list type floor))))

(define (poll-direction-buttons type)
  (for/list ([i floor-count]) (elevator-hardware-get-button-signal type i)))

;; Sends button presses to the main thread by polling the button states
;; It only sends pressed buttons to main
;; Also sets the lamp of a pressed button to "on"
(define poll-buttons (thread (lambda ()
  (let loop ()
    (sleep 0.05)
    (let-values ([(up down command) (apply values (map poll-direction-buttons elevator-hardware-button-list))])
      (for ([up* up] [down* down] [command* command] [floor floor-count])
        (map (curryr set-and-send floor) elevator-hardware-button-list (list up* down* command*)))
      (loop))))))
