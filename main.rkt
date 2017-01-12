#! /usr/bin/env racket
#lang racket

[require threading] ; This is for the ~> and ~>> operators, and has nothing to do with parallel threads

;; Useful functions
[define (not-empty list)
  [not [empty? list]]]
[define (showpipe . rest)
  [displayln rest]
  [last rest]]

;; The state each elevator contains
[struct state (position waypoints external-buttons internal-buttons resting-position) #:prefab]

;; Ensure all elevators have the same external-buttons state
[define (correct-buttons all-known-elevator-states)
  ;; Merge all lists into a single list
  [define (merge-lists list-of-lists)
    [apply append list-of-lists]]
  ;; Remove all numeric duplicates in O(n^2) time
  [define (remove-duplicates list)
    [foldr [lambda [number state] [cons number [filter [lambda [z] [not [= number z]]] state]]] empty list]]
  [~>>
    [map state-external-buttons all-known-elevator-states]
    merge-lists
    remove-duplicates]]

;; Remove floors (buttons) which we can confirm have been cleared by an elevator
[define (prune-buttons all-known-elevator-states)
  [let ([s all-known-elevator-states])
    [error "Function not yet implemented"]]]

;; Map external buttons into waypoints
[define (assign-mission all-known-elevator-states
  [let ([s all-known-elevator-states])
    [error "Function not yet implemented"]]]

;; Example
[define example-states [list
                         [state 0 '[] '[1 3] '[] 0]
                         [state 0 '[] '[] '[] 0]
                         [state 0 '[] '[9] '[] 0]
                         [state 0 '[] '[9 8 8 8 3] '[] 0]
                         [state 5 '[] '[1]   '[] 5]]]
[correct-buttons example-states]
