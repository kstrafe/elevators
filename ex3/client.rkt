#! /usr/bin/env racket
#lang racket

[require "udp-util.rkt"]

[define echo-port 20004]
[define fixed-tcp-port 34933]
[define varying-tcp-port 33546]

[define socket [udp-socket 30000]]

[let retry ()
  [let-values ([(host port message) [udp-receive socket]])
    [if [bytes=? message #"(i-am-the-server)"]
      [let ([echo-socket [udp-socket echo-port]])
        [udp-send-to echo-socket host echo-port #"(hello-server it-is-i)"]
        [let-values ([(h p m) [udp-receive echo-socket]])
          [displayln m]]
        [udp-close echo-socket]
        [let-values ([(tcp-in tcp-out) [tcp-connect host varying-tcp-port]])
          [let reader ([previous [read tcp-in]])
            [if [not [eq? eof previous]]
              [begin
                [displayln previous]
                [let ([to-write [read]])
                  [write [if [eq? to-write eof] "bye" to-write] tcp-out]]
                [flush-output tcp-out]
                [if [not [eq? eof previous]]
                  [reader [read tcp-in]]
                  empty]]
              empty]]]]
      [begin
        [displayln "The server is not authentic!"]
        [retry]]]]]

[udp-close socket]
