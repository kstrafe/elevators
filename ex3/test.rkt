#! /usr/bin/env racket
#lang racket

[require lens threading
         (for-syntax threading)]
[define-syntax (show syn)
  #`(lambda (s)
      [displayln (#,[string->symbol
        [string-append "state-" [symbol->string [cadr [syntax->datum syn]]]]] s)] s)]
[struct/lens state (continue? iteration start-time) #:prefab]
[let loop ([state `#s(state #t 0 ,(current-inexact-milliseconds))])
  [if (state-continue? state)
    [~>
      state
      [[lambda (x) [displayln x] [lens-transform state-iteration-lens x (lambda (y) (add1 y))]]]
      [[lambda (x) [lens-set state-continue?-lens x [< [state-iteration x] 3]]]]
      [[show iteration]]
      loop]
    state]]
