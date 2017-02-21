#! /usr/bin/env racket
#lang racket

(require "logger.rkt")

(define filepath "count")
(define sleep-time .5)
(define initial-ttl 3)

(define (spawn-backup)
  (displayln "Spawning backup...")
  (subprocess (current-output-port) #f 'stdout "/usr/bin/env" "racket" "phoenix.rkt"))

(define (get-count)
  (with-handlers ([exn? (lambda (e) (displayln e) 0)])
    (if (file-exists? filepath)
      (file->value filepath)
      #f)))

(define (write-count count)
  (with-handlers ([exn? (lambda (e) (displayln e))])
    (with-output-to-file filepath (lambda () (write count)) #:exists 'replace)))

(define (main-loop primary count old-count ttl)
  (sleep sleep-time)
  (let ([count* (add1 count)])
    (if primary
      (begin
        (displayln count)
        (write-count count)
        (main-loop #t count* count ttl))
      (cond
        ;; Take over as primary
        [(zero? ttl)
         (spawn-backup)
         (main-loop #t count* (get-count) initial-ttl)]
        ;; Decrement ttl if no new count is registered
        [(= count old-count) (main-loop #f count (get-count) (sub1 ttl))]
        ;; Continue registering count as backup
        [else (main-loop #f count* (get-count) ttl)]))))

(define (main)
  (displayln "Process spawned.")
  (let ([count (get-count)])
    (if count
      (main-loop #f count count initial-ttl)
      (begin
        (write-count 0)
        (spawn-backup)
        (main-loop #t 0 -1 initial-ttl)))))

(main)
