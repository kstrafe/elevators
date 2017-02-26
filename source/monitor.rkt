#lang racket

;; Open up the fifo pipe for writing
(define monitor-fifo-in
  (when (file-exists? "temporaries/monitor-fifo")
    (open-input-file "temporaries/monitor-fifo")))

;; Start a new main with backup commands as command line arguments
(define (respawn-elevator message)
  (let ([filepath "temporaries/respawned-main-output"])
    (when (file-exists? filepath)
      (delete-file filepath))
    (subprocess (open-output-file filepath #:exists 'append) #f 'stdout "/usr/bin/env" "bash" "-c"
      (string-join (list "racket main.rkt" (string-append "\"" (~a message) "\""))))))

(let loop ([message empty] [time-to-live 2000])
  (let ([message* (read monitor-fifo-in)]
        [time-to-live* (sub1 time-to-live)])
    (cond
      ([negative? time-to-live] (respawn-elevator message))
      ([eof-object? message*] (loop message time-to-live*))
      (else (loop message* time-to-live)))))
