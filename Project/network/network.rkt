#! /usr/bin/env racket
#lang racket

(provide receive send)

(require racket/async-channel
         racket/serialize
         racket/fasl)

(define broadcast-address
  (let-values ([(p o i e)
    (subprocess #f #f #f "/usr/bin/env" "bash" "-c" "ifconfig | grep Bcast | cut -d':' -f 3 | cut -d' ' -f 1")])
    (read-line o)))
(define broadcast-port 30073)
(define broadcast-sleep 0.4)

(define sender-channel (make-async-channel))
(define udp-channel (udp-open-socket))
(udp-bind! udp-channel #f broadcast-port)

(define (receive)
  (let ([input-buffer (make-bytes 65535)])
    (let-values ([(message-length source-host source-port) (udp-receive!* udp-channel input-buffer)])
      (if message-length
        (cons (fasl->s-exp input-buffer) (receive))
        empty))))

(define (send info)
  (async-channel-put sender-channel info))

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

