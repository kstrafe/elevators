#lang racket

(provide floor-reached? set-motor-to)

(require racket/async-channel "elevator-hardware/elevator-interface.rkt")

(define (floor-reached?) (async-channel-try-get done-channel))
(define (set-motor-to floor) (async-channel-put sender-channel floor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sender-channel (make-async-channel))
(define done-channel   (make-async-channel))

(define poll (thread (lambda ()
  (let loop ([target-floor 0] [previous-floor 0])
    (sleep 0.10)
    (let ([desired-floor (async-channel-try-get sender-channel)])
      (if desired-floor
        (loop desired-floor previous-floor)
        (let* ([current    (elevator-hardware-get-floor-sensor-signal)]
               [current*   (if (negative? current) previous-floor current)]
               [difference (- target-floor current*)])
          (elevator-hardware-set-motor-direction
            (cond
              [(positive? difference) 'DIRN_UP]
              [(zero?     difference) (when (not (= previous-floor target-floor)) (async-channel-put done-channel target-floor)) 'DIRN_STOP]
              [(negative? difference) 'DIRN_DOWN]))
          (loop target-floor current*))))))))
