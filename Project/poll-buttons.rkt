#lang racket

(provide poll-buttons)

(require "elevator-hardware/elevator-interface.rkt"
         "utilities.rkt")

(define floor-count 4)

(define poll-buttons (thread (lambda ()
  (let loop ([previous-floor 0]
             [previous-obstruct #t])
    (sleep 0.05)
    (let ([buttons-up (for/list ([i floor-count]) (elevator-hardware-get-button-signal 'BUTTON_CALL_UP i))]
          [buttons-down (for/list ([i floor-count]) (elevator-hardware-get-button-signal 'BUTTON_CALL_DOWN i))]
          [buttons-command (for/list ([i floor-count]) (elevator-hardware-get-button-signal 'BUTTON_COMMAND i))]
          [floor (elevator-hardware-get-floor-sensor-signal)]
          [stop? (elevator-hardware-get-stop-signal)]
          [obstruction? (elevator-hardware-get-obstruction-signal)])
      (if (= obstruction? 0)
        (begin
          (when previous-obstruct
            (trce `("Obstruction (reset all lamps)" ,obstruction?))
            (for ([i 4])
              (elevator-hardware-set-button-lamp 'BUTTON_CALL_UP i 0)
              (elevator-hardware-set-button-lamp 'BUTTON_CALL_DOWN i 0)
              (elevator-hardware-set-button-lamp 'BUTTON_COMMAND i 0))
            (elevator-hardware-set-floor-indicator 0)
            (elevator-hardware-set-door-open-lamp 0)
            (elevator-hardware-set-stop-lamp 0))
          (loop 0 #f))
        (begin
          (for ([state-up buttons-up]
                [state-down buttons-down]
                [state-command buttons-command]
                [floor floor-count])
            (when (= state-up 1) (elevator-hardware-set-button-lamp 'BUTTON_CALL_UP floor state-up))
            (when (= state-down 1) (elevator-hardware-set-button-lamp 'BUTTON_CALL_DOWN floor state-down))
            (when (= state-command 1) (elevator-hardware-set-button-lamp 'BUTTON_COMMAND floor state-command)))
          (when (= stop? 1)
            (trce `("Set stop lamp to" ,stop?))
            (elevator-hardware-set-stop-lamp 1))
          (if (and (positive? floor) (not (= floor previous-floor)))
            (begin
              (trce `("Set floor lamp to" ,floor))
              (elevator-hardware-set-floor-indicator floor)
              (loop floor #t))
            (loop previous-floor #t)))
      ))))))

(sleep 100)
