#lang racket

(require "logger.rkt")

;; Open up the fifo pipe
(define monitor-fifo-in
  (if (file-exists? "temporaries/monitor-fifo")
    (open-input-file "temporaries/monitor-fifo")
    (erro "Monitor fifo not found")))

(let loop ()
  (read monitor-fifo-in)
  (loop))
