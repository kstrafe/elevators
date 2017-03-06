#lang racket

(require racket/async-channel "logger.rkt" "try-get-last.rkt")

(define initial-time-to-live 5)

;; Open up the fifo pipe for writing
(define monitor-fifo-in
  (when (file-exists? "temporaries/monitor-fifo")
    (open-input-file "temporaries/monitor-fifo")))

;; Start a new main with backup commands as command line arguments
(define (respawn-elevator message)
  (system* "/usr/bin/env" "pkill" "-9" "-f" "main.rkt")
  (warn "You can't stump the Trump")
  (subprocess (current-output-port) (current-input-port) (current-error-port) "/usr/bin/env" "bash" "-c"
    (string-join (list "racket main.rkt" (string-append "\"" (~a message) "\"")))))

;; Create an async channel to put data from fifo in
(define fifo-channel (make-async-channel))

;; Create a thread that constantly tries to read from the fifo asynchronously
(define fifo-reader (thread (lambda ()
  (let loop () (async-channel-put fifo-channel (read monitor-fifo-in)) (loop)))))

;; Main loop of the monitor.
;;
;; Tries to get a message read from fifo asynchronously.
;; If it cannot read a value from fifo, a time-to-live counter
;; is decremented on the next loop.
;; When time-to-live becomes negative or EOF is received any
;; old elevator is killed and a new one is spawned.
;; Else the loop continues with its new message.
(let loop ([message empty] [time-to-live initial-time-to-live])
  ; (trce time-to-live)
  (let ([message* (async-channel-try-get-last#io fifo-channel #f)]
        [time-to-live* (sub1 time-to-live)])
    (sleep 0.1)
    (cond
      ([or (negative? time-to-live) (eof-object? message*)] (respawn-elevator message))
      ([false? message*] (loop message time-to-live*))
      (else (loop message* initial-time-to-live)))))
