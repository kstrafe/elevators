#lang racket

(require "logger.rkt")

;; Open up the fifo pipe
(define monitor-fifo-in (open-input-file "/tmp/monitor-fifo"))

;; TODO Bug remains where this blocks the other process
(let loop ()
  (dbug* (read monitor-fifo-in))
  (loop))
