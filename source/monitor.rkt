#lang racket

;; Open up the fifo pipe
(define monitor-fifo-in
  (when (file-exists? "temporaries/monitor-fifo")
    (open-input-file "temporaries/monitor-fifo")))

;; Kill the elevator if it's still running, start a new one and kill this process
(define (respawn-elevator message)
  (with-handlers ([exn? (lambda (e) (with-output-to-file "temporaries/err" (lambda () (displayln e)) #:exists 'replace))])
    (subprocess (open-output-file "temporaries/resurrected-main" #:exists 'append) #f 'stdout "/usr/bin/env" "bash" "-c" "racket main.rkt")))

(let loop ([message empty] [time-to-live 2000])
  (with-handlers ([exn? (lambda (e) (with-output-to-file "temporaries/err" (lambda () (displayln e)) #:exists 'replace))])
    (with-output-to-file "temporaries/fifo-in" (lambda () (displayln (cons message time-to-live))) #:exists 'append)
    (let ([message* (read monitor-fifo-in)]
          [time-to-live* (sub1 time-to-live)])
      (with-output-to-file "temporaries/message" (lambda () (writeln message*)) #:exists 'append)
      (cond
        ([negative? time-to-live] (respawn-elevator message))
        ([eof-object? message*] (loop message time-to-live*))
        (else (loop message* time-to-live))))))
