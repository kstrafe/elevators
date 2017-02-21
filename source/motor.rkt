#lang racket

(provide any-new-floor-reached? is-blocked? move-to-floor)

(require racket/async-channel "elevator-hardware/elevator-interface.rkt" "logger.rkt")

(define (any-new-floor-reached?) (async-channel-try-get-last  status-channel #f))
(define (is-blocked?)            (async-channel-try-get-last  blocked-channel #f))
(define (move-to-floor floor)    (async-channel-put           motor-channel  floor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get the last value in the channel.
;; If the channel is completely empty, return old-value.
(define (async-channel-try-get-last channel old-value)
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
    (let* ([desired-floor  (async-channel-try-get-last motor-channel target-floor)]
           [current        (elevator-hardware:get-floor-sensor-signal)]
           [current*       (if (negative? current) previous-floor current)]
           [difference*    (- target-floor current*)])
      (when (not (= previous-floor current*)) (async-channel-put status-channel current*))

      ;; The motor is apparently not reliable enough and requires us
      ;; to spam it a bit :/

      ; (when (not (= difference difference*))
        (elevator-hardware:set-motor-direction
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
