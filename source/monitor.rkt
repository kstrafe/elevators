#lang racket

(let loop ()
  (if #t
    ; (displayln (port->string))
    ;; TODO This causes the entire program to freeze, figure out how to read from the input port
    (port->string)
    (displayln '#f))
  (loop))
