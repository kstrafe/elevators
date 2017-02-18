#lang racket

(provide broadcast receive)

(require racket/async-channel racket/fasl sha "logger.rkt")

;; Broadcast a message
;;
;; Wraps the message 'm' -> (('m' timestamp) hash)
;; Where the 'hash' is the hash of the fasl-serialized ('m' timestamp)
;; The hash is checked by the receiver.
;; The message is broadcast continuously by a thread. It runs
;; at a speed defined by broadcast-sleep.
(define (broadcast info)
  (let ([message (list info (current-inexact-milliseconds))])
    (async-channel-put broadcast-channel (list message (hashify message)))))

;; Receive messages
;;
;; Unwraps a message of the form (('m' timestamp) hash) by
;; checking the hash against the fasl-serialized ('m' timestamp).
;; If the hash is OK, then ('m' timestamp) is accepted
;; and put into a list of (('m1' timestamp1) ('m2' timestamp2) ...).
;; This list is returned.
(define (receive)
  (define max-messages-per-receive 500)
  (define (subreceive have-received)
    (if (> have-received max-messages-per-receive)
      empty
      (let ([input-buffer (make-bytes 65535)])
        (let-values ([(message-length source-host source-port) (udp-receive!* udp-channel input-buffer)])
          (if message-length
            (with-handlers ([exn? (lambda (e) (receive))])
              (let ([message (fasl->s-exp input-buffer)])
                (if (hash-check message)
                  (cons (first message) (subreceive (add1 have-received)))
                  (subreceive have-received))))
            empty)))))
  (subreceive 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define broadcast-address
  (let-values ([(program out in err)
    ;; TODO Use a more reliable method here
    ;; Using a subprocess works, but sometimes there are other network
    ;; entities also serving Bcast, which means that this network module
    ;; broadcasts to the wrong network.
    (subprocess #f #f #f "/usr/bin/env" "bash" "-c" "ifconfig | grep Bcast | cut -d':' -f 3 | cut -d' ' -f 1")])
    (let ([error (port->string err)])
      (when (non-empty-string? error)
        (ftal "Unable to get broadcast address" error)
        (exit 1)))
    (read-line out)))
(define-values (broadcast-port broadcast-sleep) (values 30073 0.04))

;; Check if a message's hash is the same as the has of its data
(define (hash-check message) (bytes=? (hashify (first message)) (second message)))

;; Serialize and then hash a message
(define (hashify message) (sha256 (s-exp->fasl message)))

;; Create an asynchronous channel, and open a UDP socket
(define-values (broadcast-channel udp-channel) (values (make-async-channel) (udp-open-socket)))
(udp-bind! udp-channel #f broadcast-port #t)

;; Continuously broadcasts until a new value is supplied by the broadcast function
(define broadcast-thread (thread (lambda ()
  (let loop ([to-send #f])
    (let ([new-value (async-channel-try-get broadcast-channel)])
      (if new-value
        (loop new-value)
        (begin
          (when to-send
            (udp-send-to udp-channel broadcast-address broadcast-port (s-exp->fasl to-send)))
          (sleep broadcast-sleep)
          (loop to-send))))))))
