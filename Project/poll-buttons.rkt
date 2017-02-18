#lang racket

(provide pop-button-states set-lights-using-commands)

(require lens racket/async-channel "data-structures.rkt"
  "elevator-hardware/elevator-interface.rkt" "logger.rkt"
  "utilities.rkt")

;; Get all button presses. Remove duplicates. Add timestamps.
(define (pop-button-states)
  (map (curry set-command-timestamp (current-inexact-milliseconds))
    (remove-duplicates
      (let loop ()
        (let ([button (async-channel-try-get button-channel)])
          (if button
            (cons (rename button) (loop))
            empty))))))

;; Set the current elevator's lights using the button states
(define (set-lights-using-commands elevator)
  (let ([external-commands (elevator-state-external-requests elevator)]
        [internal-commands (elevator-state-internal-requests elevator)])
    (let ([commands (append external-commands internal-commands)])
      (map (lambda (c) (elevator-hardware-set-button-lamp (button-type c) (button-floor c) 1)) commands))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (button-floor a-command)
  (if (external-command? a-command)
    (external-command-floor a-command)
    (internal-command-floor a-command)))

(define button-channel (make-async-channel))

(define (set-command-timestamp time command)
  (if (internal-command? command)
    (lens-set internal-command-timestamp-lens command time)
    (lens-set external-command-timestamp-lens command time)))

(define (button-type a-command)
  (if (external-command? a-command)
    (if (symbol=? (external-command-direction a-command) 'up)
      'BUTTON_CALL_UP
      'BUTTON_CALL_DOWN)
    'BUTTON_COMMAND))

(define (rename type)
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
  (let loop ([previous-floor    0]
             [previous-obstruct #t])
    (sleep 0.05)
    (let-values ([(buttons-up buttons-down buttons-command) (apply values (map poll-direction-buttons elevator-hardware-button-list))])
            ;; TODO Remove floor sensor here, lamp-setting of floor sensors can be implemented using state-position
            ;; Motor already gives us floor signals, and is better suited for it
      (let ([floor (elevator-hardware-get-floor-sensor-signal)]
            [stop? (if (= 1 (elevator-hardware-get-stop-signal)) #t #f)]
            [obstruction? (if (= 0 (elevator-hardware-get-obstruction-signal)) #t #f)])
        (if obstruction?
          (begin
            (when previous-obstruct
              (trce `("Obstruction (reset all lamps)" ,obstruction?))
              (map (lambda (type) (for ([i floor-count]) (elevator-hardware-set-button-lamp type i 0))) elevator-hardware-button-list)
              (elevator-hardware-set-floor-indicator 0)
              (elevator-hardware-close-door)
              (elevator-hardware-set-stop-lamp 0))
            (loop 0 #f))
          (begin
            (for ([state-up buttons-up]
                  [state-down buttons-down]
                  [state-command buttons-command]
                  [floor floor-count])
              (map (curryr set-and-send floor) elevator-hardware-button-list (list state-up state-down state-command)))
            (when stop?
              (trce "Stop lamp pressed")
              (elevator-hardware-set-stop-lamp 1))
            (if (and (not (negative? floor)) (not (= floor previous-floor)))
              (begin
                (trce `("Set floor lamp to" ,floor))
                (elevator-hardware-set-floor-indicator floor)
                (loop floor #t))
              (loop previous-floor #t))))))))))
