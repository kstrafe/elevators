#lang racket

(provide utilities^)

(define-signature utilities^
  (this:state
   this:opening
   this:command
   this:call
   this:position
   this:done
   this:servicing
   other:servicing
   if-changed-call
   read-commands#io
   first-or-empty
   map-hash-table
   hash-remove-predicate
   same-request?
   prune-requests
   make-empty-elevator
   compute-available-call-requests
   compute-direction-of-travel
   compute-direction-to-travel
   score-elevator-request
   elevator-request-direction
   process-available-call-requests
   insert-button-presses-into-this-elevator-as-requests
   filter-newest-to-hash
   unify-messages-and-elevators
   insert-self-into-elevators
   remove-all-dead-elevators
   decrement-all-time-to-live
   update-position#io
   unify-requests
   prune-call-requests-that-are-done
   assign-call-requests
   store-commands#io
   service-commands
   sort-servicing
   set-motor-direction-to-task#io
   prune-done-requests
   prune-servicing-requests
   detect-and-remove-floor-cycle
   check-for-fatal-situations))
