#lang racket

(provide (all-defined-out))

(require lens)

(define max-done-length       30)
(define floor-count           9)
(define door-open-iterations  20)
(define iteration-sleep-time  0.1)
(define time-to-live          (ceiling (/ 6 iteration-sleep-time)))

(define-syntax-rule (struct/lens-es (name (attributes ...) keywords ...) ...)
  (begin (struct/lens name (attributes ...) #:prefab keywords ...) ...))

(struct/lens-es
  (complex     (floors calls commands elevators monitor-fifo-out))
  (attributes  (state time-to-live timestamp))
  (state       (id name position servicing-requests call-requests command-requests done-requests opening-time))
  (request     (direction floor timestamp)))

(define (command-request? request) (symbol=? (request-direction request) 'command))
(define (call-request? request) (not (symbol=? (request-direction request) 'command)))
