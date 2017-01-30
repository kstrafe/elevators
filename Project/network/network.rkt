#! /usr/bin/env racket
#lang racket

[provide receive send]

[require racket/async-channel
         racket/serialize
         racket/fasl]

[define broadcast-port 30073]
[define broadcast
  [let-values [([p o i e]
    [subprocess #f #f #f "/usr/bin/env" "bash" "-c" "ifconfig | grep Bcast | cut -d':' -f 3 | cut -d' ' -f 1"])]
    [read-line o]]]

[define udp-channel [udp-open-socket]]
[udp-bind! udp-channel #f broadcast-port]

[define channel [make-async-channel 5000]]

[define queuer (thread (lambda ()
  [let loop ([input-buffer [make-bytes 1024]])
    [let-values ([(message-length source source-port) [udp-receive! udp-channel input-buffer]])
      [async-channel-put channel [subbytes input-buffer 0 message-length]]
      [loop input-buffer]]]))]

[define (receive)
  [let ([received-value [async-channel-try-get channel]])
    [if received-value
      [cons [fasl->s-exp received-value] [receive]]
      empty]]]

[define (send info)
  [udp-send-to udp-channel broadcast broadcast-port [s-exp->fasl info]]]
