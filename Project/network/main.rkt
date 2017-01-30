#! /usr/bin/env racket
#lang racket

[require "network.rkt"]

[let loop ([n 0])
  [when [= [modulo n 2] 0]
    [send `(external-floor-request ,n)]]
  [let ([listing [receive]])
    [displayln `(size ,[length listing] ,listing)]]
  [sleep 0.5]
  [loop [add1 n]]]
