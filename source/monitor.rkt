#lang racket

;; Open up the fifo pipe for writing
(define monitor-fifo-in
  (when (file-exists? "temporaries/monitor-fifo")
    (open-input-file "temporaries/monitor-fifo")))

;; Create a backup fifo to send message and start an new main
(define (respawn-elevator message)
  (when (not (file-exists? "temporaries/commands-backup-fifo"))
    (system* "/usr/bin/env" "mkfifo" "temporaries/commands-backup-fifo"))
  (write message (open-output-file "temporaries/commands-backup-fifo" #:exists 'replace))
  (subprocess (open-output-file "temporaries/respawned-main-output" #:exists 'append) #f 'stdout "/usr/bin/env" "bash" "-c" "racket main.rkt"))

(let loop ([message empty] [time-to-live 2000])
  (let ([message* (read monitor-fifo-in)]
        [time-to-live* (sub1 time-to-live)])
    (cond
      ([negative? time-to-live] (respawn-elevator message))
      ([eof-object? message*] (loop message time-to-live*))
      (else (loop message* time-to-live)))))
