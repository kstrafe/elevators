#lang racket

;;;; Control the motor and access the floor status.
;;;; The module simplifies the hardware interface by allowing us to state a floor to
;;;; go to.

(provide any-new-floor-reached?#io is-blocked?#io move-to-floor#io)

(require racket/async-channel "try-get-last.rkt" "elevator-hardware/elevator-interface.rkt" "logger.rkt")

(define (any-new-floor-reached?#io)  (async-channel-try-get-last#io  status-channel   #f))
(define (is-blocked?#io)             (async-channel-try-get-last#io  blocked-channel  #f))
(define (move-to-floor#io floor)     (async-channel-put              motor-channel    floor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define blocked-channel  (make-async-channel))
(define motor-channel    (make-async-channel))
(define status-channel   (make-async-channel))
(define poll-interval    0.100)
(define motor-blocked    5000)

;; Poll the elevator floor whilst giving the elevator directions to reach its destination floor
(define poll (thread (lambda ()
  ; These values make the elevator go down to the ground floor when started
  (let loop ([target-floor 0] [previous-floor -1] [difference 0] [previous-difference 0] [time (current-inexact-milliseconds)])
    (let* ([desired-floor  (async-channel-try-get-last#io motor-channel target-floor)]
           [current        (elevator-hardware:get-floor-sensor-signal#io)]
           [current*       (if (negative? current) previous-floor current)]
           [difference*    (- target-floor current*)])
      (when (not (= previous-floor current*)) (async-channel-put status-channel current*))
      (elevator-hardware:set-motor-direction#io
        (cond
          [(positive?  difference) 'DIRN_UP]
          [(zero?      difference) 'DIRN_STOP]
          [(negative?  difference) 'DIRN_DOWN]))
      (sleep poll-interval)
      (if (and (= previous-difference difference) (not (zero? difference)))
        (begin
          (when (> (- (current-inexact-milliseconds) time) motor-blocked)
            (async-channel-put blocked-channel #t))
          (loop desired-floor current* difference* difference time))
        (loop desired-floor current* difference* difference (current-inexact-milliseconds))))))))
