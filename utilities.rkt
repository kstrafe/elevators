#lang racket

(provide (all-defined-out))

(require lens racket/fasl racket/hash racket/pretty racket/syntax rackunit rackunit/text-ui threading
  "data-structures.rkt" "motor.rkt" "logger.rkt" (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Read commands from file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPURE : This procedure is impure as it reads from a file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (read-commands!)
  (with-handlers ([exn? (lambda (e) (displayln e) empty)])
    (let ([filepath "commands"]) (if (file-exists? filepath) (file->value filepath) empty))))

;; Get the first of a list or return empty
(define (first-or-empty list) (if (empty? list) empty (first list)))

;; Map each value in a hash-table
(define (map-hash-table hash function)
  (for/hash ([(k v) (in-hash hash)]) (values k (function v))))

;; Remove all key-value pairs where (predicate value) is true
(define (hash-remove-predicate hash predicate)
  (for/hash ([(k v) (in-hash hash)] #:when (not (predicate v))) (values k v)))

;; Is the request the same (disregarding timestamp)
(define (same-request? left right)
  (and (symbol=? (request-direction left) (request-direction right))
       (= (request-floor left) (request-floor right))))

;; Unify new and stored -commands into a list where new-commands that already exist in stored-commands are excluded
(define (prune-requests new-commands stored-commands)
  (~>
    (foldl
      (lambda (req accum)
        (if (ormap (curry same-request? req) stored-commands) accum (cons req accum)))
      empty new-commands)
    reverse
    (append stored-commands)))

(define-syntax-rule (check-equals? call ((input ...) result) ...)
  (begin (check-equal? (call input ...) result) ...))

(define prune-requests-tests
  (test-suite "Test prune-requests"
    (check-equals? prune-requests
      (('() '())                                                                  '())
      (('(#s(request up 0 0)) '(#s(request up 0 0)))            '(#s(request up 0 0)))
      (('(#s(request up 1 0)) '(#s(request up 1 0)))            '(#s(request up 1 0)))
      (('(#s(request up 1 2)) '(#s(request up 1 0)))            '(#s(request up 1 0)))
      (('(#s(request up 1 0)) '(#s(request up 1 2)))            '(#s(request up 1 2)))

      (('(#s(request down 0 0)) '(#s(request up 0 0)))          '(#s(request down 0 0) #s(request up 0 0)))
      (('(#s(request up 0 0)) '(#s(request down 0 0)))          '(#s(request up 0 0) #s(request down 0 0)))
      (('(#s(request down 0 1)) '(#s(request down 0 0)))        '(#s(request down 0 0)))

      (((for/list ([i (range 50 150)]) (request 'command i 0)) (for/list ([i 100]) (request 'command i 0)))
        (append (for/list ([i (range 100 150)]) (request 'command i 0)) (for/list ([i 100]) (request 'command i 0)))))))
(run-tests prune-requests-tests)


(define done-of-hash (lens-compose state-done-requests-lens attributes-state-lens))
(define call-of-hash (lens-compose state-call-requests-lens attributes-state-lens))

;; Create a new empty elevator
(define (make-empty-elevator id name)
  (attributes (state id name 0 empty empty (read-commands!) empty 0) time-to-live (current-inexact-milliseconds)))

;; Calculate which call requests are available to pick
;;
;; This algorithm checks if a call request is already
;; being serviced by an elevator. If it is, it's not available.
(define (compute-available-call-requests hash)
  (define other:servicing (lens-compose state-servicing-requests-lens attributes-state-lens))
  (define other:call      (lens-compose state-call-requests-lens attributes-state-lens))
  (let ([elevators (hash-values hash)])
    (~>
      (map (curry lens-view other:servicing) elevators)
      flatten
      ; (remove* _ (flatten (map (curry lens-view other:call) elevators)))
      (remove* _ (flatten (lens-view other:call (first elevators))))
      (filter call-request? _)
      ; remove-duplicates
      flatten)))

;; Compute a state's current direction of travel based on its
;; position and currently servicing requests
(define (compute-direction-of-travel state)
  (compute-direction-to-travel state (state-servicing-requests state)))

;; Compute the direction to travel from a state to a list of requests
(define (compute-direction-to-travel state requests)
  (if (empty? requests)
    'halt
    (let ([position (state-position state)]
          [request  (first requests)])
      (cond
        ([< position (request-floor request)]         'up)
        ([> position (request-floor request)]         'down)
        ([symbol=? (request-direction request) 'up]   'up)
        ([symbol=? (request-direction request) 'down] 'down)
        (else      (compute-direction-to-travel state (rest requests)))))))

;; Compute an elevator's score
;; Computes the distance from an elevator to a request
(define (score-elevator-request state request)
  (abs (- (state-position state) (request-floor request))))

;; Find the direction of an elevator's current top request
(define (elevator-request-direction elevator)
  (let loop ([requests (state-servicing-requests elevator)])
    (if (empty? requests)
      'halt
      (let ([direction (request-direction (first requests))])
        (if (symbol=? direction 'command)
          (loop (rest requests))
          direction)))))

;; Assign available call requests to the best possible elevators
;;
;; This function computes all call requests for all elevators. This computation
;; makes sure that all elevators arrive at the exact same assigned calls given
;; the same input elevators and requests.
;; The function tries to optimize distance to a floor in addition to time since a
;; button press.
;; The tie breaker is the elevator's UUID. If both elevators score equally, the one
;; with the lexicographically lesser UUID gets to take that call.
(define (process-available-call-requests elevators requests)
  (define any:state attributes-state-lens)
  (let ([top-request (first-or-empty requests)])
    (if (empty? top-request)
      elevators
      (let ([best-id
        (~>
          (map (curry lens-view any:state) (hash-values elevators))
          (map (lambda (elevator)
            (let ([elevator* (list elevator)])
              (append
                elevator*
                (list
                  (compute-direction-of-travel     elevator)
                  (compute-direction-to-travel     elevator (list top-request))
                  (request-direction               top-request)
                  (elevator-request-direction      elevator)
                  (state-position                  elevator)
                  (request-floor                   top-request))))) _)
          (filter (lambda (elev-info)
            (or
              (and
                (match (second elev-info)
                  ('up   (< (sixth elev-info) (seventh elev-info)))
                  ('down (> (sixth elev-info) (seventh elev-info)))
                  (_ #t))
                (symbol=? (second elev-info) (third  elev-info))
                (symbol=? (second elev-info) (fourth elev-info))
                (or (symbol=? (fourth elev-info) (fifth elev-info))))
              (symbol=? (second elev-info) 'halt))) _)
          (map first _)
          (map (lambda (elevator) (list (state-id elevator) (score-elevator-request elevator top-request))) _)
          (sort string<? #:key first)
          ((lambda (lst) (if (empty? lst) empty (argmin second lst))))
          first-or-empty)])
        (process-available-call-requests
          (if (empty? best-id)
            elevators
            (lens-transform (other:servicing best-id) elevators (curry cons top-request)))
          (rest requests))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core pipe algorithms                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Insert the button presses to the current elevator's state
(define (insert-button-presses-into-this-elevator-as-requests buttons elevators)
  (let* ([command-requests  (filter command-request? buttons)]
         [elevators*        (lens-transform this:command elevators (curry prune-requests command-requests))]
         [call-requests     (filter call-request? buttons)]
         [elevators**       (lens-transform this:call elevators* (curry prune-requests call-requests))])
    elevators**))

;; Turn messages into a hash-table of attributes (exactly like the elevators hash-table)
;; This keeps only the latest version of a message
;; The 'messages' var is of the form ((msg timestamp) ...)
(define (filter-newest-to-hash messages)
  (foldl (lambda (msg accum)
    (let* ([id         (state-id (first msg))]
           [timestamp  (last msg)])
      (if (or (not (hash-has-key? accum id)) (> timestamp (attributes-timestamp (hash-ref accum id))))
        (hash-set accum id (attributes (first msg) time-to-live timestamp))
        accum)))
    (make-immutable-hash)
    messages))

;; Hash-table union of elevators and messages containing elevators
;; Each message and elevator (in elevators) has a timestamp.
;; Sometimes messages are delayed, so the current elevator timestamp is larger
;; than a message. We must thus discard this old message.
(define (unify-messages-and-elevators messages elevators)
  (let ([ts attributes-timestamp])
    (hash-union messages elevators #:combine (lambda (elev-1 elev-2) (if (> (ts elev-1) (ts elev-2)) elev-1 elev-2)))))

;; This elevator is inside the elevators hash-table, messages normally update all elevators
;; in this hash-table, but we don't want to rely on a message from itself to itself (because it can get lost)
;; so we manually insert this elevator into the elevators hash-table to ensure that it will
;; always exist.
(define (insert-self-into-elevators elevators folded-elevators)
  (lens-set this:state elevators (lens-view this:state folded-elevators)))

;; Remove all elevators whose time-to-live value is negative
(define (remove-all-dead-elevators elevators)
  (hash-remove-predicate elevators (lambda (elevator) (negative? (attributes-time-to-live elevator)))))

;; Decrement 'time-to-live' in for each elevator
(define (decrement-all-time-to-live elevators)
  (map-hash-table elevators (lambda (x) (lens-transform attributes-time-to-live-lens x sub1))))

;; Update the position number in the current elevator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPURE : This procedure is impure as it reads a value coming from the detectors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-position! elevators)
  (if (is-blocked?)
    (begin
      (crit "Motor is blocked")
      (sleep 1)
      (update-position! elevators))
    (let ([floor (any-new-floor-reached?)])
      (if floor
        (lens-set this:position elevators floor)
        elevators))))

;; Unify (create a list containing all unique) done-requests from all elevators
;; Also unify all call-requests from all elevators.
;; All elevators then have the exact same done-requests and call-requests lists.
(define (unify-requests elevators)
  (define (unify-requests* elevators accessor)
    (~>
      (map (curry lens-view accessor) (hash-values elevators))
      flatten
      remove-duplicates
      (sort < #:key request-timestamp)
      (define unified _))
    (map-hash-table elevators (lambda (x)
      (lens-set accessor x unified))))
  (~>
    (unify-requests* elevators done-of-hash)
    (unify-requests* call-of-hash)))

;; Remove the requests from call-requests that are also in done-requests
;;
;; Set all call-requests lists in all elevators to be the same pruned version.
;; Pruned in the sense that all done-requests that are also in call-requests
;; are removed from call-requests.
(define (prune-call-requests-that-are-done elevators)
  (let* ([state   (first (hash-values elevators))]
         [done    (lens-view done-of-hash state)]
         [calls   (lens-view call-of-hash state)]
         [calls*  (foldr (lambda (call accum) (if (ormap (curry equal? call) done) accum (cons call accum))) empty calls)])
    (map-hash-table elevators (lambda (attribute) (lens-set call-of-hash attribute calls*)))))

;; Assign call requests to elevators
;;
;; Filters out all feasible call requests (requests that aren't already taken by other elevators)
;; and runs through each of them to find out which elevator has the least distance from that request.
;; In addition to this, the elevator is checked for having the same motion (if it had any)
;; as the call request, and that the elevator is below/above the request depending on the request.
(define (assign-call-requests elevators)
  (process-available-call-requests elevators (compute-available-call-requests elevators)))

;; Store commands locally, to be restored in case of power loss
;;
;; Writes the commands to a file. In case of any power loss, the elevator
;; should read the file to restore all commands that were queued.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPURE : This procedure is impure as it writes to a file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (store-commands! elevators)
  (with-handlers ([exn? (lambda (error) (crit error))])
    (with-output-to-file "commands" (lambda () (pretty-write (lens-view this:command elevators))) #:exists 'replace))
  elevators)

;; Compute which commands are going to be serviced by checking the current position
;; and the current heading. We can find out which commands to process. These commands
;; are prepended to the servicing requests list and later sorted by another function.
(define (service-commands elevators)
  (let* ([position (lens-view this:position elevators)]
         [servicing (lens-view this:servicing elevators)]
         [commands (lens-view this:command elevators)]
         [direction (compute-direction-of-travel (lens-view this:state elevators))])
      (let loop
          ([eligible
            (remove* servicing
              (cond
                ([symbol=? direction 'halt]  commands)
                ([symbol=? direction 'up]    (filter (lambda (x) (> (request-floor x) position)) commands))
                ([symbol=? direction 'down]  (filter (lambda (x) (< (request-floor x) position)) commands))))])
        (if (not (empty? eligible))
          (let ([best (argmin request-timestamp eligible)])
            (service-commands (lens-transform this:servicing elevators (curry cons best))))
          elevators))))

;; Sort the currently servicing requests because we want to service floors in between
;; our original destination. Say you're going from 3 to 8, and then a new request comes
;; #s(request up 6 ...), then this request will be added such that our servicing list
;; is (3 8 6), so we sort to (3 6 8), which services the six in-between 3 and 8.
;;
;; It is assumed that assign-call-requests and service-commands assign
;; requests in such a way that sorting the requests will not
;; make the elevator turn around (cause a floor cycle).
(define (sort-servicing elevators)
  (let ([direction (compute-direction-of-travel (lens-view this:state elevators))])
    (lens-transform this:servicing elevators
      (lambda (x)
        (if (symbol=? direction 'halt)
          x
          (sort x
            (cond
              ([symbol=? direction 'up] <)
              ([symbol=? direction 'down] >))
            #:key request-floor))))))

;; Calls move-to-floor depending on the top of the 'servicing-requests'
;; field in the current elevator.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPURE : This procedure is impure as it sets the direction of the elevator motor ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (set-motor-direction-to-task! elevators)
  (let ([servicing-requests (lens-view this:servicing elevators)])
    (when (not (empty? servicing-requests))
      (~>
        (first servicing-requests)
        request-floor
        move-to-floor)))
  elevators)

;; Ensure that done-requests doesn't grow without bounds
;;
;; The done-requests list (of this elevator) can grow until it reaches
;; a maximum size. If it goes above this size, the list is trimmed from
;; the end, meaning that the oldest requests are removed.
(define (prune-done-requests elevators)
  (lens-transform this:done elevators
    (lambda (list)
      (if (> (length list) max-done-length)
        (take list max-done-length)
        list))))

;; Remove requests which have been served and inform the state to open the doors
;;
;; If we find this elevator on the same floor as one of its requests that it's currently servicing, then
;; this elevator must open its doors and remove this request.
(define (prune-servicing-requests elevators)
  (let ([servicing (first-or-empty (lens-view this:servicing elevators))])
    (if (and (not (empty? servicing)) (= (lens-view this:position elevators) (request-floor servicing)))
      (~>
        (lens-transform this:servicing elevators rest)
        (lens-transform this:done      _ (lambda (done)     (if (call-request? servicing)    (cons servicing done)       done)))
        (lens-transform this:command   _ (lambda (internal) (if (command-request? servicing) (remove servicing internal) internal)))
        (lens-set this:opening         _ door-open-iterations)
        prune-servicing-requests)
      elevators)))

;; Floor cycles occur when the elevator is servicing (say) floor 4 and 6, and is currently at floor 5
;; The sorting of these requests will flip the direction during each iteration,
;; making the motor go up and down all the time.
;;
;; This should never happen according to the logic we have defined.
;; Our rules always check if the state would be legitimate and not have any cycles.
;; However, if it ever comes to that, that a cycle is detected, we clean
;; the servicing request list by removing all requests from it whilst
;; logging a critical message.
(define (detect-and-remove-floor-cycle elevators)
  (let ([position (list (lens-view this:position elevators))])
    (lens-transform this:servicing elevators
      (lambda (servicing)
        (if (not (empty? servicing))
          (let ([servicing* (map request-floor (sort servicing < #:key request-floor))])
            (if (or (apply <= (append position servicing*)) (apply <= (append servicing* position)))
              servicing
              (begin
                (crit "Servicing has a floor cycle, emptying servicing" servicing)
                empty)))
          servicing)))))

;; Check that position is within allowed limits.
;; This includes checking that position is not below zero,
;; and that position is not higher than the highest possible floor.
(define (check-for-fatal-situations elevators)
  (let ([position (lens-view this:position elevators)])
    (if (or (negative? position) (>= position floor-count))
      (begin
        (ftal "Position is not reachable without serious injury" position)
        (exit 1))
      elevators)))
