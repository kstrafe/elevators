#lang racket

;; UDP
(require racket/udp)

(define client (udp-open-socket))
(udp-bind! client #f 30000)
(define server (udp-open-socket))
(udp-bind! server #f 20004)

(define str (make-bytes 128))
(define-values (length ip port) (udp-receive! client str))
(displayln str)

(udp-send-to server ip 20004 #"Hello")

(udp-receive! server str)
(displayln str)

;; TCP
(require racket/tcp)

(define-values (in-port out-port) (tcp-connect ip 34933))

(read-line in-port)

(write '(connect-to "10.22.70.146") out-port)

(close-input-port in-port)
(close-output-port out-port)

(let ([listener (tcp-listen 34934 4 #t #f)])
    (let-values ([(i o) (tcp-accept listener)])
        (display "Hey, that's pretty good\n" o)
        (close-input-port i)
        (close-output-port o)))
