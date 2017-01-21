#! /usr/bin/env racket
#lang racket

[require "udp-util.rkt"]

[define broadcast-port 30000]
[define echo-port 20004]
[define fixed-tcp-port 34933]
[define varying-tcp-port 33546]

[define broadcaster [thread [lambda ()
  [let loop ([socket [udp-socket broadcast-port]])
    [sleep 1.5]
    [displayln "Broadcasting..."]
    [udp-send-to socket "192.168.1.255" broadcast-port #"(i-am-the-server)"]
    [loop socket]]]]]

[define echoer [thread [lambda ()
  [let loop ([socket [udp-socket echo-port]])
    [let-values ([(host port message) [udp-receive socket]])
      [udp-send-to socket host echo-port [bytes-append #"You said: " message]]
      [loop socket]]]]]]

[define tcper [thread [lambda ()
  [let loop ([listener [tcp-listen varying-tcp-port]])
    [let-values ([(in out) [tcp-accept listener]])
      [write "Welcome to this wonderful server! How may I help you today?\n" out]
      [flush-output out]
      [let responder ([input [read in]])
        [displayln input]
        [if [eq? eof input]
          empty
          [cond
            [(and (string? input) (string=? input "bye")) empty]
            [else [begin
              [match input
                [`(connect-to ,host ,port) [displayln `(,host ,port)]]
                [_ empty]]
              [write input out]
              [flush-output out]
              [responder [read in]]]]]]]
      [close-output-port out]
      [close-input-port in]
      [displayln "Closed the tcp port"]
      [loop listener]]]]]]

[define tcper-fixed [thread [lambda ()
  [let loop ([listener [tcp-listen fixed-tcp-port]])
    [let-values ([(in out) [tcp-accept listener]])
      [write "Welcome to this wonderful server! How may I help you today?\n" out]
      [write [read in] out]
      [flush-output out]
      [close-output-port out]
      [close-input-port in]
      [loop listener]]]]]]

[thread-wait echoer]
[thread-wait broadcaster]
