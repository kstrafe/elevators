#lang racket

(provide receive send)

(require racket/async-channel racket/fasl racket/serialize "../utilities.rkt")

(define broadcast-address
  (let-values ([(p o i e)
    (subprocess #f #f #f "/usr/bin/env" "bash" "-c" "ifconfig | grep Bcast | cut -d':' -f 3 | cut -d' ' -f 1")])
    (read-line o)))
(define-values (broadcast-port broadcast-sleep) (values 30073 0.4))

(define-values (sender-channel udp-channel) (values (make-async-channel) (udp-open-socket)))
(udp-bind! udp-channel #f broadcast-port)

(define (hash-check-passes message)
  (bytes=? (hashify (first message)) (second message)))

(define (receive)
  (let ([input-buffer (make-bytes 65535)])
    (let-values ([(message-length source-host source-port) (udp-receive!* udp-channel input-buffer)])
      (if message-length
        (with-handlers ([exn? (lambda (e) (receive))])
          (let ([message (fasl->s-exp input-buffer)])
            (if (hash-check-passes message)
              (cons (first message) (receive))
              (receive))))
        empty))))

(define (send info)
  (let ([message (list info (current-inexact-milliseconds))])
    (async-channel-put sender-channel (list message (hashify message)))))

(define sender-thread (thread (lambda ()
  (let loop ([to-send #f])
    (let ([new-value (async-channel-try-get sender-channel)])
      (if new-value
        (loop new-value)
        (begin
          (when to-send
            (udp-send-to udp-channel broadcast-address broadcast-port (s-exp->fasl to-send)))
          (sleep broadcast-sleep)
          (loop to-send))))))))
