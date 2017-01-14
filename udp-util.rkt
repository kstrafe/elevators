#lang racket

[provide udp-socket udp-receive]

[define (udp-socket port)
  [let ([socket [udp-open-socket]])
    [udp-bind! socket #f port]
    socket]]
[define (udp-receive socket)
  [let ([buffer [make-bytes 1024]])
    [let-values ([(size host port) [udp-receive! socket buffer]])
      [values host port [subbytes buffer 0 size]]]]]
