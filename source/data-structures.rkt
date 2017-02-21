#lang racket

(provide (all-defined-out))

(require lens "identity-generator.rkt" "logger.rkt")

(define max-done-length       30)
(define floor-count           9)
(define door-open-iterations  20)
(define iteration-sleep-time  0.1)
(define time-to-live          (ceiling (/ 6 iteration-sleep-time)))

(define-syntax-rule (struct/lens-es (name (attributes ...) keywords ...) ...)
  (begin (struct/lens name (attributes ...) #:prefab keywords ...) ...))

(struct/lens-es
  (complex     (floors calls commands elevators))
  (attributes  (state time-to-live timestamp))
  (state       (id name position servicing-requests call-requests command-requests done-requests opening-time))
  (request     (direction floor timestamp)))

(define (command-request? request) (symbol=? (request-direction request) 'command))
(define (call-request? request) (not (symbol=? (request-direction request) 'command)))

;; Load/generate an identity
(define-values (id name) (generate-or-load-identity#io))
(info id name)

(define this:state     (lens-compose attributes-state-lens (hash-ref-lens id)))
(define this:opening   (lens-compose state-opening-time-lens attributes-state-lens (hash-ref-lens id)))
(define this:command   (lens-compose state-command-requests-lens this:state))
(define this:call      (lens-compose state-call-requests-lens this:state))
(define this:position  (lens-compose state-position-lens this:state))
(define this:done      (lens-compose state-done-requests-lens this:state))
(define this:servicing (lens-compose state-servicing-requests-lens this:state))

(define (other:servicing id) (lens-compose (lens-compose state-servicing-requests-lens attributes-state-lens (hash-ref-lens id))))
