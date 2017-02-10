#lang racket

(provide pop-button-states)

(require racket/async-channel
         "elevator-hardware/elevator-interface.rkt"
         "utilities.rkt")

(define floor-count 4)

(define button-channel (make-async-channel))

;; Get all button presses. Remove duplicates. Add timestamps.
(define (pop-button-states)
  (map (curryr append `(,(current-inexact-milliseconds)))
    (remove-duplicates
      (let loop ()
        (let ([button (async-channel-try-get button-channel)])
          (if button
            (cons (rename button) (loop))
            empty))))))

(define (remove-duplicates list) (foldr (lambda (x s) (cons x (filter (lambda (z) (not (equal? x z))) s))) empty list))

(define (rename type)
  (match type
    [(list 'BUTTON_CALL_UP floor) `(up ,floor)]
    [(list 'BUTTON_CALL_DOWN floor) `(down ,floor)]
    [(list 'BUTTON_COMMAND floor) `(command ,floor)]
    [_ type]))

(define (set-and-send type state floor)
  (when (= state 1)
    (elevator-hardware-set-button-lamp type floor state)
    (async-channel-put button-channel (list type floor))))

(define (poll-direction-buttons type)
  (for/list ([i floor-count]) (elevator-hardware-get-button-signal type i)))

;; Sends button presses to the main thread by polling the button states (pressed or unpressed)
;; It only sends pressed buttons to main
;; Also sets the lamp of a pressed button to "on"
;; The purpose of this code is to give responsive button presses
(define poll-buttons (thread (lambda ()
  (let loop ([previous-floor 0]
             [previous-obstruct #t])
    (sleep 0.05)
    (let-values ([(buttons-up buttons-down buttons-command) (apply values (map poll-direction-buttons elevator-hardware-button-list))])
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
              (trce `("Set stop lamp to" ,stop?))
              (async-channel-put button-channel '(stop))
              (elevator-hardware-set-stop-lamp 1))
            (if (and (positive? floor) (not (= floor previous-floor)))
              (begin
                (trce `("Set floor lamp to" ,floor))
                (elevator-hardware-set-floor-indicator floor)
                (loop floor #t))
              (loop previous-floor #t))))))))))
