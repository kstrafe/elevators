#lang racket

[define-namespace-anchor base]
[define ns [namespace-anchor->namespace base]]

[require racket/udp]

;; UDP
[define sender [udp-open-socket]]
[udp-bind! sender #f 30000]
[udp-send-to sender "10.22.70.146" 30000 #"Hello, this is I, the racket server"]
[displayln "Sent welcome"]

[define receiver [udp-open-socket]]
[udp-bind! receiver #f 20004]
[define received-bytes [make-bytes 128]]
[displayln "Waiting for response"]
[define-values (len ip port) (udp-receive! receiver received-bytes)]
[udp-send-to receiver ip 20004 [bytes-append #"You said: " received-bytes]]
[displayln received-bytes]
[displayln "Sent reply"]

;; TCP
[require racket/tcp]

[define (connect-to ip)
  [sleep 1]
  [let-values [[(i o) [tcp-connect ip 34934]]]
    [display [read-line i]]
    [close-output-port o]
    [close-input-port i]]]

[define listener [tcp-listen 34933 10 #t #f]]
[define-values (in out) [tcp-accept listener]]
[displayln "Writing to tcp"]
[display "Welcome to the serverln\n" out]
[flush-output out]
[eval [read in] ns]
[displayln "Closing ports"]
[close-output-port out]
[close-input-port in]
[tcp-close listener]
[displayln "Ended"]
