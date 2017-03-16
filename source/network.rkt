#lang racket

;;;; This module handles all communication with the other elevators over UDP.
;;;; It is also responsible for creating and maintaining a UDP connection when it is made available, regardless of
;;;; network status when the elevator is started.

(provide broadcast#io receive#io)

(require racket/async-channel racket/fasl sha "logger.rkt" "try-get-last.rkt")

;; Broadcast a message.
;;
;; Wraps the message 'm' -> (('m' timestamp) hash) where the 'hash' is the hash of the fasl-serialized ('m' timestamp).
;; The hash is checked by the receiver.
;; The message is broadcast continuously by a thread. It runs at a speed defined by broadcast-sleep.
(define (broadcast#io info)
  (let ([message (list info (current-inexact-milliseconds))])
    (async-channel-put broadcast-channel (list message (hashify message)))))

;; Receive messages.
;;
;; Unwraps a message of the form (('m' timestamp) hash) by checking the hash against the serialized ('m' timestamp).
;; If the hash is OK, then ('m' timestamp) is accepted and put into a list of (('m1' timestamp1) ('m2' timestamp2) ...).
;;
;; This list is returned.
(define (receive#io)
  (with-handlers ([exn? (lambda (error) (trce "Got exn in receive#io" error) (receive#io))])
    (define max-messages-per-receive 500)
    (define (subreceive have-received input-buffer)
      (if (> have-received max-messages-per-receive)
        empty
        (let-values ([(message-length source-host source-port) (udp-receive!* udp-channel input-buffer)])
          (if message-length
            (let ([message (fasl->s-exp input-buffer)])
              (if (hash-check message)
                (cons (first message) (subreceive (add1 have-received) input-buffer))
                (subreceive (add1 have-received) input-buffer)))
            empty))))
    (subreceive 0 (make-bytes 65535))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal helpers                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (broadcast-port broadcast-sleep) (values 30073 0.04))

;; Turn lines into a list of strings
(define (lines->list port) (let ([ip (read-line port)]) (if (eof-object? ip) empty (cons ip (lines->list port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Continuously write the freshest broadcast addresses to this channel.
;;
;; This allows us to easily continue working even if there are many broadcast addresses.
(define broadcast-addresses-channel (make-async-channel))
(define broadcast-addresses (thread (lambda () (let retry ()
  (let-values ([(program out in err)
                (subprocess #f #f #f
                            "/usr/bin/env" "bash" "-c" "ifconfig | grep Bcast | cut -d':' -f 3 | cut -d' ' -f 1")])
    (let ([error (port->string err)])
      (when (non-empty-string? error) (crit "Unable to get broadcast address" error)))
    (let ([ip (lines->list out)])
      (close-output-port  in)
      (close-input-port   out)
      (close-input-port   err)
      (if (not (empty? ip))
        (begin (async-channel-put broadcast-addresses-channel (append (file->value "resources/ips") ip)) (sleep 5))
        (begin (warn* "Unable to find broadcast addresses, retrying in one second") (async-channel-put broadcast-addresses-channel (append (file->value "resources/ips") ip)) (sleep 1)))
      (retry)))))))

;; Create an asynchronous channel and open a UDP socket
(define-values (broadcast-channel udp-channel) (values (make-async-channel) (udp-open-socket)))
(udp-bind! udp-channel #f broadcast-port #t)

;; Continuously broadcasts until a new value is supplied by the broadcast function
(define broadcast-thread (thread (lambda ()
  (let loop ([to-send #f] [addresses #f])
    (let ([to-send*    (async-channel-try-get-last#io broadcast-channel            to-send)]
          [addresses*  (async-channel-try-get-last#io broadcast-addresses-channel  addresses)])
      (when (and to-send addresses)
        (with-handlers ([exn:fail:network? erro*])
          (for ([address addresses*])
            (udp-send-to udp-channel address broadcast-port (s-exp->fasl to-send*)))))
      (sleep broadcast-sleep)
      (loop to-send* addresses*))))))

;; Check if a message's hash is the same as the hash of its data
(define (hash-check message) (bytes=? (hashify (first message)) (second message)))

;; Serialize and then hash a message
(define (hashify message) (sha256 (s-exp->fasl message)))
