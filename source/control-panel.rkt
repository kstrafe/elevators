#lang racket

(provide pop-button-states#io set-call-lights#io set-command-lights#io set-floor-indicator#io)

(require racket/async-channel lens "data-structures.rkt" "elevator-hardware/elevator-interface.rkt" "logger.rkt")

;; Get all button presses, remove duplicates and add timestamps
(define (pop-button-states#io)
  (map (curry set-command-timestamp (current-inexact-milliseconds))
       (remove-duplicates
         (let loop ()
           (let ([button (async-channel-try-get button-channel)])
             (if button (cons (buttonify button) (loop)) empty))))))

(define (set-call-lights#io calls)
  (let* ([calls-up    (map request-floor (filter (lambda (elev) (symbol=? (request-direction elev) 'up))    calls))]
         [calls-down  (map request-floor (filter (lambda (elev) (symbol=? (request-direction elev) 'down))  calls))])
    (for ([floor (range floor-count)])
      (elevator-hardware:set-button-lamp#io 'BUTTON_CALL_DOWN  floor (if (ormap (curry = floor) calls-down)  1 0))
      (elevator-hardware:set-button-lamp#io 'BUTTON_CALL_UP    floor (if (ormap (curry = floor) calls-up)    1 0)))))

(define (set-command-lights#io commands)
  (let* ([commands* (map request-floor commands)])
    (for ([floor (range floor-count)])
      (elevator-hardware:set-button-lamp#io 'BUTTON_COMMAND  floor (if (ormap (curry = floor) commands*) 1 0)))))

(define (set-floor-indicator#io floor) (elevator-hardware:set-floor-indicator#io floor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a channel that transports button presses to main
(define button-channel (make-async-channel))

;; Translate a raw hardware button into command structs
(define (buttonify type)
  (match type
    [(list 'BUTTON_CALL_UP    floor) (request 'up       floor 0)]
    [(list 'BUTTON_CALL_DOWN  floor) (request 'down     floor 0)]
    [(list 'BUTTON_COMMAND    floor) (request 'command  floor 0)]
    [_ type]))

;; Sends button presses to the main thread by polling the button states.
;;
;; It only sends pressed buttons to main.
;; Also sets the lamp of a pressed button to "on".
(define poll-buttons (thread (lambda ()
  (let loop ([old-commands (build-list floor-count (lambda _ 0))])
    (sleep 0.05)
    (let-values ([(up down command) (apply values (map poll-direction-buttons#io elevator-hardware:button-list))])
      (for ([up* up] [down* down] [command* command] [floor floor-count] [old-command* old-commands])
        (map (curryr set-and-send#io floor)
             elevator-hardware:button-list
             (list up* down* (if (> command* old-command*) 1 0))))
      (loop command))))))

;; Find out which buttons are currently pressed and not
(define (poll-direction-buttons#io type) (for/list ([i floor-count]) (elevator-hardware:get-button-signal#io type i)))

;; Set a button's light on and inform main about this button
(define (set-and-send#io type state floor)
  (when (= state 1)
    (elevator-hardware:set-button-lamp#io type floor state)
    (async-channel-put button-channel (list type floor))))

;; Sets a timestamp on a command
(define (set-command-timestamp time request) (lens-set request-timestamp-lens request time))
