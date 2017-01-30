#! /usr/bin/env racket
#lang racket

[require racket/tcp]

[error 'here "What"]

[let loop ([listener [tcp-listen 30000 4 #t]])
  [define-values [in out] [tcp-accept listener]]
  [displayln [read-line in]]
  [close-input-port in]
  [close-output-port out]
  [loop listener]]
