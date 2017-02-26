#lang racket

(provide async-channel-try-get-last#io)

(require racket/async-channel)

;; Get the last value in the channel.
;; If the channel is completely empty, return old-value.
(define (async-channel-try-get-last#io channel old-value)
  (let loop ([previous old-value])
    (let ([value (async-channel-try-get channel)])
      (if value
        (loop value)
        previous))))
