#! /usr/bin/env racket
#lang racket

;; The state each elevator contains
[struct state (position waypoints external-buttons internal-buttons resting-position) #:prefab]

;; Ensure all elevators have the same external-buttons state
[define (correct-buttons all-known-elevator-states)
  [let ([s all-known-elevator-states])
    [error "Function not yet implemented"]]]

;; Remove floors (buttons) which we can confirm have been cleared by an elevator
[define (prune-buttons all-known-elevator-states)
  [let ([s all-known-elevator-states])
    [error "Function not yet implemented"]]]

;; Map external buttons into waypoints
[define (assign-mission all-known-elevator-states
  [let ([s all-known-elevator-states])
    [error "Function not yet implemented"]]]
