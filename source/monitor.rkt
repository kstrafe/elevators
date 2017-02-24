#lang racket

;; Open up the fifo pipe for writing
(define monitor-fifo-in
  (when (file-exists? "temporaries/monitor-fifo")
    (open-input-file "temporaries/monitor-fifo")))

;; Kill the elevator if it's still running, start a new one and kill this process
(define (respawn-elevator message)
  (subprocess (open-output-file "temporaries/respawned-main-output" #:exists 'append) #f 'stdout "/usr/bin/env" "bash" "-c" "racket main.rkt"))

(let loop ([message empty] [time-to-live 2000])
  (let ([message* (read monitor-fifo-in)]
        [time-to-live* (sub1 time-to-live)])
    (cond
      ([negative? time-to-live] (respawn-elevator message))
      ([eof-object? message*] (loop message time-to-live*))
      (else (loop message* time-to-live)))))
