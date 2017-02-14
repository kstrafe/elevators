#! /usr/bin/env racket
#lang racket

; raco pkg install lens threading libuuid <<< a
(require lens racket/hash threading "elevator-hardware/elevator-interface.rkt" "identity-generator.rkt" "network/network.rkt" "poll-buttons.rkt" "utilities.rkt")

;; Fold the button presses into the current elevator. Then put the current elevator into all-elevators
(define (fold-buttons-into-elevators buttons this-elevator all-elevators)
  (let* ([button-presses buttons]
         [commands (filter (lambda (x) (eq? (first x) 'command)) button-presses)]
         [this-elevator* (foldl (lambda (c s) (lens-transform elevator-state-internal-requests-lens s (lambda (x) (cons c x)))) this-elevator commands)]
         [non-commands (filter (lambda (x) (not (eq? (first x) 'command))) button-presses)]
         [this-elevator** (foldl (lambda (c s) (lens-transform elevator-state-external-requests-lens s (lambda (x) (cons c x)))) this-elevator* non-commands)]
         [all-elevators* (hash-set all-elevators (elevator-state-id this-elevator) (elevator-attributes this-elevator time-to-live (current-inexact-milliseconds)))])
    (values this-elevator all-elevators*)))

;; Ensure that we use the incremental garbage collector
(collect-garbage 'incremental)

(struct/lens elevator-state (id name position servicing-requests external-requests internal-requests done-requests resting-position opening-time) #:prefab)
(struct/lens elevator-attributes (state time-to-live timestamp) #:prefab)

(define (elevator-attributes-refresh attributes)
  (elevator-attributes (elevator-attributes-state attributes) time-to-live (current-inexact-milliseconds)))

(define-values (id name) (generate-identity))
(info id name)

(define initial-elevator-state (elevator-state id name 0 empty empty empty empty 0 empty))
(define time-to-live 3)

(define (filter-newest this-elevator all-elevators messages)
  (~>
    ;; Filter all messages on id and newest time
    (foldl (lambda (c s)
        (cond [(not (hash-has-key? s (elevator-state-id (first c)))) (hash-set s (elevator-state-id (first c)) (elevator-attributes (first c) 3 (last c)))]
              [(> (last c) (elevator-attributes-timestamp (hash-ref s (elevator-state-id (first c))))) (hash-set s (elevator-state-id (first c)) (elevator-attributes (first c) 3 (last c)))]
              [else s]))
      (make-immutable-hash)
      messages)
    ;; Discard messages older than current from all-elevators
    (hash-union all-elevators #:combine/key (lambda (k a b) (if (> (elevator-attributes-timestamp a) (elevator-attributes-timestamp b)) a b)))
    ;; Update our own current state
    (hash-set id (elevator-attributes this-elevator 3 (current-inexact-milliseconds)))))

(let loop ([this-elevator initial-elevator-state]
           [all-elevators (make-immutable-hash)])
  (send this-elevator)
  (sleep 1)

  (let-values ([(this-elevator* all-elevators*) (fold-buttons-into-elevators (pop-button-states) this-elevator all-elevators)])
    (let ([messages (receive)])
      (trce all-elevators*)
      ;; TODO For each ID, time needs to be compared to each other AND all-elevators* time
      (if #f
        (~>
          ;; Remove all old messages
          (filter-newest this-elevator* all-elevators* messages)

          ;; Decrement all 'time-to-live's
          (map-hash-table (lambda (x)
            (lens-transform elevator-attributes-time-to-live-lens x sub1)))

          ;; Reset 'time-to-live' for the elevators from which we received a message
          (hash-set-from-list _ elevator-state-id in: messages update-with: (lambda (msg)
            (elevator-attributes msg time-to-live (current-inexact-milliseconds))))

          ;; Refresh time-to-live and timestamp for ourselves
          ((lambda (x) (hash-set x id (elevator-attributes-refresh (hash-ref x id)))))

          ;; Remove all elevators where time-to-live <= 0
          (hash-remove-predicate (lambda (x) (<= (elevator-attributes-time-to-live x) 0)))

          (loop this-elevator* _))
        (loop this-elevator* all-elevators*)))))
