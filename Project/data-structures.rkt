#lang racket

(provide (all-defined-out))

(require lens "identity-generator.rkt" "logger.rkt")

(define floor-count           4)
(define iteration-sleep-time  0.1)
(define time-to-live          (ceiling (/ 6 iteration-sleep-time)))

(define-syntax-rule (struct/lens-es (name (attributes ...) keywords ...) ...)
  (begin (struct/lens name (attributes ...) #:prefab keywords ...) ...))

(struct/lens-es
  (attributes  (state time-to-live timestamp))
  (state       (id name position servicing-requests call-requests command-requests completed-call-requests opening-time))
  (request     (direction floor timestamp)))

(define (command-request? request) (symbol=? (request-direction request) 'command))
(define (call-request? request) (not (symbol=? (request-direction request) 'command)))

;; Load/generate an identity
(define-values (id name) (generate-or-load-identity))
(info id name)

(define state-lens (lens-compose attributes-state-lens (hash-ref-lens id)))
(define opening-lens (lens-compose state-opening-time-lens attributes-state-lens (hash-ref-lens id)))
(define command-requests-lens (lens-compose state-command-requests-lens state-lens))
(define call-lens (lens-compose state-call-requests-lens state-lens))
(define position-lens (lens-compose state-position-lens state-lens))
(define done-lens (lens-compose state-completed-call-requests-lens state-lens))
(define servicing-lens (lens-compose state-servicing-requests-lens state-lens))
