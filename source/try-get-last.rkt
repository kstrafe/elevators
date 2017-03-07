#lang racket

;;;; Provides a function for getting the last new value from an asynchronous channel.
;;;;
;;;; This module provides a utility-io function. motor.rkt uses this function, but utilities.io uses motor.rkt.
;;;; It has its own module to prevent cyclic dependencies.

(provide async-channel-try-get-last#io)

(require racket/async-channel)

;; Get the last value in the channel.
;;
;; If the channel is completely empty, return the old value.
(define (async-channel-try-get-last#io channel old-value)
  (let loop ([previous old-value])
    (let ([value (async-channel-try-get channel)])
      (if value (loop value) previous))))
