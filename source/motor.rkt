#lang racket

(provide any-new-floor-reached?#io is-blocked?#io move-to-floor#io)

(require racket/async-channel "elevator-hardware/elevator-interface.rkt" "logger.rkt")

(define (any-new-floor-reached?#io) (async-channel-try-get-last#io status-channel #f))
(define (is-blocked?#io)            (async-channel-try-get-last#io  blocked-channel #f))
(define (move-to-floor#io floor)    (async-channel-put           motor-channel  floor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get the last value in the channel.
;; If the channel is completely empty, return old-value.
(define (async-channel-try-get-last#io channel old-value)
  (let loop ([previous old-value])
    (let ([value (async-channel-try-get channel)])
      (if value
        (loop value)
        previous))))

(define blocked-channel  (make-async-channel))
(define motor-channel    (make-async-channel))
(define status-channel   (make-async-channel))

(define poll (thread (lambda ()
  ; These values make the elevator go down to the ground floor when started
  (let loop ([target-floor 0] [previous-floor 1] [difference 0] [previous-difference 0] [time (current-inexact-milliseconds)])
    (let* ([desired-floor  (async-channel-try-get-last#io motor-channel target-floor)]
           [current        (elevator-hardware:get-floor-sensor-signal#io)]
           [current*       (if (negative? current) previous-floor current)]
           [difference*    (- target-floor current*)])
      (when (not (= previous-floor current*)) (async-channel-put status-channel current*))

      ;; The motor is apparently not reliable enough and requires us
      ;; to spam it a bit :/

      ; (when (not (= difference difference*))
        (elevator-hardware:set-motor-direction#io
          (cond
            [(positive? difference) 'DIRN_UP]
            [(zero?     difference) 'DIRN_STOP]
            [(negative? difference) 'DIRN_DOWN]))
      ;)

      (sleep 0.20)
      (if (and (= previous-difference difference) (not (= difference 0)))
        (begin
          (when (> (- (current-inexact-milliseconds) time) 5000)
            (async-channel-put blocked-channel #t))
          (loop desired-floor current* difference* difference time))
        (loop desired-floor current* difference* difference (current-inexact-milliseconds))))))))
