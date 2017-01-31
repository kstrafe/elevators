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

[define (receive)
  [let ([input-buffer [make-bytes 1024]])
    [let-values ([(message-length source-host source-port) [udp-receive!* udp-channel input-buffer]])
      [if message-length
        [cons [fasl->s-exp input-buffer] [receive]]
        empty]]]]

[define (send info)
  [udp-send-to udp-channel broadcast broadcast-port [s-exp->fasl info]]]
