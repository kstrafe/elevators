#lang racket

;;;; The monitor is its own program. It checks whether main is functioning properly.
;;;; If it is not, this program restarts main.

(require racket/async-channel "logger.rkt" "try-get-last.rkt")

;; The amount of iterations we tolerate of main not sending us data
(define initial-time-to-live 10)

;; Open up the fifo pipe for writing
(define monitor-fifo-in (when (file-exists? "temporaries/monitor-fifo") (open-input-file "temporaries/monitor-fifo")))

;; Start a new main with backup commands as command line arguments
(define (respawn-elevator message)
  (system* "/usr/bin/env" "pkill" "-9" "-f" "main.rkt")
  (warn "You can't stump the Trump")
  (subprocess (open-output-file "temporaries/new-main-out" #:exists 'replace) #f 'stdout
              "/usr/bin/env" "bash" "-c" (string-join (list "racket main.rkt" (string-append "\"" (~a message) "\"")))))

;; Create an async channel to put data from fifo in
(define fifo-channel (make-async-channel))

;; Create a thread that constantly tries to read from the fifo asynchronously
(define fifo-reader (thread (lambda () (let loop () (async-channel-put fifo-channel (read monitor-fifo-in)) (loop)))))

;; Main loop of the monitor.
;;
;; Tries to get a message read from fifo asynchronously.
;; If it cannot read a value from fifo, a time-to-live counter
;; is decremented on the next loop.
;; When time-to-live becomes negative or EOF is received any
;; old elevator is killed and a new one is spawned.
;; Else the loop continues with its new message.
(let loop ([message empty] [time-to-live initial-time-to-live])
  (let ([message*       (async-channel-try-get-last#io fifo-channel #f)]
        [time-to-live*  (sub1 time-to-live)])
    (sleep 0.1)
    (cond
      ([or (negative? time-to-live) (eof-object? message*)]  (respawn-elevator message))
      ([false? message*]                                     (loop message time-to-live*))
      (else                                                  (loop message* initial-time-to-live)))))
