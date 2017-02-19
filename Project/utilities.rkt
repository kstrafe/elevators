#lang racket

(provide (all-defined-out))

(require lens racket/fasl racket/hash racket/pretty racket/syntax rackunit rackunit/text-ui threading
  "data-structures.rkt" "motor.rkt" "logger.rkt" (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Read commands from file
(define (read-commands)
  (with-handlers ([exn? (lambda (e) (displayln e) empty)])
    (let ([filepath "commands"]) (if (file-exists? filepath) (file->value filepath) empty))))

;; Get the first of a list or return empty
(define (first-or-empty list) (if (empty? list) empty (first list)))

;; Map each value in a hash-table
(define (map-hash-table hash function)
  (foldl (lambda (x s) (hash-set s x (function (hash-ref s x)))) hash (hash-keys hash)))

;; Remove all key-value pairs where (predicate value) is true
(define (hash-remove-predicate hash predicate)
  (foldl (lambda (x s) (if (predicate (hash-ref s x)) (hash-remove s x) s)) hash (hash-keys hash)))

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
  (attributes (state id name 0 empty empty (read-commands) empty 0) time-to-live (current-inexact-milliseconds)))

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

;; Sorting function that sorts on id if the scores are equal
(define (id-score-sort id-score-1 id-score-2)
  (cond
    ([< (second id-score-1) (second id-score-2)] #t)
    ([> (second id-score-1) (second id-score-2)] #f)
    ([string<? (first id-score-1) (first id-score-2)] #t)
    ([string>=? (first id-score-1) (first id-score-2)] #f)))

;; Find the direction of an elevator's request
(define (elevator-request-direction elevator)
  (let ([request (first-or-empty (state-servicing-requests elevator))])
    (if (empty? request)
      'halt
      (request-direction request))))

;; Assign available call requests to the best possible elevators
(define (process-available-call-requests hash requests)
  (define any:state attributes-state-lens)
  (let ([top-request (first-or-empty requests)])
    (if (empty? top-request)
      hash
      (begin
        (let ([best-id
            (~>
              (map (curry lens-view any:state) (hash-values hash))
              (map (lambda (x) (list x (compute-direction-of-travel x))) _)
              (map (lambda (x) (append x (list (compute-direction-to-travel (first x) (list top-request))))) _)
              (map (lambda (x) (append x (list (request-direction top-request)))) _)
              (map (lambda (x) (append x (list (elevator-request-direction (first x))))) _)
              (map (lambda (x) (append x (list (state-position (first x))))) _)
              (map (lambda (x) (append x (list (request-floor top-request)))) _)
              ;; If we have request higher than current, and we're NOT in halt, then we MUST drop it
              (filter
                (lambda (x)
                  (or
                    (and
                      (match (second x)
                        ('up (< (sixth x) (seventh x)))
                        ('down (> (sixth x) (seventh x)))
                        (_ #t))
                                                      ; dot = direction of travel
                      (symbol=? (second x) (third x)) ; = current-dot dot-top-request
                      (symbol=? (second x) (fourth x)) ; = current-dot direction-top-request
                      (symbol=? (fourth x) (fifth x))) ; = direction-top-request current-top-request
                    (symbol=? (second x) 'halt))) _)
              (map first _)
              (map (lambda (x) (list (state-id x) (score-elevator-request x top-request))) _)
              (sort id-score-sort)
              first-or-empty
              first-or-empty)])
          (process-available-call-requests
            (if (empty? best-id)
              hash
              (lens-transform (other:servicing best-id) hash (curry cons top-request)))
            (rest requests))
          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipe algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add the button presses to the current elevator's state
(define (fold-buttons-into-elevators buttons elevators)
  (let* ([command-requests  (filter command-request? buttons)]
         [elevators*        (lens-transform this:command elevators (curry prune-requests command-requests))]
         [call-requests     (filter call-request? buttons)]
         [elevators**       (lens-transform this:call elevators* (curry prune-requests call-requests))])
    elevators**))

;; Turn messages make into hash-table of attributes
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

;; Union of elevators and messages, where the newest timestamp gets to stay
(define (unify-messages-and-elevators messages elevators)
  (let ([ts attributes-timestamp])
    (hash-union messages elevators #:combine (lambda (a b) (if (> (ts a) (ts b)) a b)))))

;; Force our own elevator to be inserted, regardless of a received message
(define (insert-self-into-elevators elevators folded-elevators)
  (lens-set this:state elevators (lens-view this:state folded-elevators)))

;; Remove all elevators whose time-to-live value is negative
(define (remove-dead-elevators elevators)
  (hash-remove-predicate elevators (lambda (x) (negative? (attributes-time-to-live x)))))

;; Decrement all 'time-to-live's
(define (decrement-time-to-live elevators)
  (map-hash-table elevators (lambda (x) (lens-transform attributes-time-to-live-lens x sub1))))

;; Update the position number in the current elevator
(define (update-position hash)
  (let ([floor (any-new-floor-reached?)])
    (if floor
      (lens-set this:position hash floor)
      hash)))

;; Unify all done-requests from all elevators
;; Also unify all call-requests from all elevators
(define (unify-requests elevators)
  (define (unify-requests* all-elevators accessor)
    (~>
      (map (curry lens-view accessor) (hash-values all-elevators))
      flatten
      remove-duplicates
      (sort < #:key request-timestamp)
      (define unified-dones _))
    (map-hash-table all-elevators (lambda (x)
      (lens-set accessor x unified-dones))))
  (~>
    (unify-requests* elevators done-of-hash)
    (unify-requests* call-of-hash)))

;; Remove the requests from call-requests that are also in done-requests
(define (prune-call-requests-that-are-done elevators)
  (let* ([state   (first (hash-values elevators))]
         [done    (lens-view done-of-hash state)]
         [calls   (lens-view call-of-hash state)]
         [calls*  (foldr (lambda (call accum) (if (ormap (curry equal? call) done) accum (cons call accum))) empty calls)])
    (map-hash-table elevators (lambda (attribute) (lens-set call-of-hash attribute calls*)))))

;; Assign call requests to elevators
(define (assign-call-requests hash)
  (process-available-call-requests hash (compute-available-call-requests hash)))

;; Store commands locally, to be restored in case of power loss
(define (store-commands elevators)
  (with-handlers ([exn? (lambda (e) (displayln e))])
    (with-output-to-file "commands" (lambda () (write (lens-view this:command elevators))) #:exists 'replace))
  elevators)

;; Compute which commands are going to be serviced
(define (service-commands elevators)
  (let* ([state (lens-view this:state elevators)]
         [position (lens-view this:position elevators)]
         [servicing (lens-view this:servicing elevators)]
         [commands (lens-view this:command elevators)]
         [opening (lens-view this:opening elevators)]
         [direction (compute-direction-of-travel state)])
    (~>
      ;; Move internal request to servicing request if standing still
      (if (and (not (empty? commands)) (symbol=? direction 'halt))
        (let ([oldest-command (foldl (lambda (c s) (if (< (request-timestamp c) (request-timestamp s)) c s)) (first commands) (rest commands))])
          (lens-set this:servicing elevators (remove-duplicates (cons oldest-command servicing))))
        elevators)
      ;; Add all eligible commands to servicing
      (lens-set this:servicing _
        (~>
          (filter
            (lambda (x)
              (or
                (symbol=? direction 'halt)
                (and (symbol=? direction 'up) (> (request-floor x) position))
                (and (symbol=? direction 'down) (< (request-floor x) position))))
            commands)
          (append servicing)
          remove-duplicates)))))

;; Sort the currently servicing requests
;;
;; It is assumed that assign-call-requests assigns
;; in such a way that sorting the requests will not
;; make the elevator turn around.
(define (sort-servicing hash)
  (let ([direction (compute-direction-of-travel (lens-view this:state hash))])
    (dbug direction)
    (lens-transform this:servicing hash
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
;
;; The ! at the end of the function name denotes side-effect without altering hash.
;; Maybe we should in general separate side effects from changing the hash table.
(define (set-motor-direction-to-task! hash)
  (let ([servicing-requests (lens-view this:servicing hash)])
    (when (not (empty? servicing-requests))
      (~>
        (first servicing-requests)
        request-floor
        move-to-floor)))
  hash)

;; Ensure that done-requests doesn't grow without bounds
(define (prune-done-requests hash)
  (lens-transform this:done hash
    (lambda (list)
      (if (> (length list) max-done-length)
        (take list max-done-length)
        list))))

;; Remove requests which have been served and inform the state
;; to open the doors
(define (prune-servicing-requests hash)
  (let ([servicing (first-or-empty (lens-view this:servicing hash))])
    (if (and (not (empty? servicing)) (= (lens-view this:position hash) (request-floor servicing)))
      (~>
        (lens-transform this:servicing hash rest)
        (lens-transform this:done      _ (lambda (done) (if (call-request? servicing) (cons servicing done) done)))
        (lens-transform this:command   _ (lambda (internal) (if (command-request? servicing) (remove servicing internal) internal)))
        (lens-set this:opening         _ door-open-iterations)
        prune-servicing-requests)
      hash)))
