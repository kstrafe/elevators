# Elevators

Elevator project written for the course `TTK4145` (Real-time Programming) at Norwegian University of Science and Technology (NTNU).

## Description

The elevator control software is to steer several (or one) elevators such that they service all requests regardless of single failures.
Elevators also need some (sub-)optimal allocation algorithm to ensure that a good path is followed (least energy, least time,...).

* Elevators that are disconnected from the network will still need to serve their orders.
* An elevator that shuts down and restarts must restore internal (command) orders.
* Requests must _always_ be handled in finite time.

### Design

The design is *extremely* data-oriented. We started contemplating the required information in order for elevators to make
good decisions. We arrived at the following data structure:

```
(state (
	id                  ; A UUID. It will be stored, so restarting will give you the same UUID. Reset it by clearing temporaries.
	name                ; A human-readable name given to the elevator (to make communication and detection easier)
	position            ; The last known position of the elevator (in-between floors has no position)
	servicing-requests  ; The requests that an elevator is currently servicing
	call-requests       ; The available call requests (buttons from outside the elevator) for all elevators
	command-requests    ; The available command requests (buttons from inside the elevator), only for the elevator holding it
	done-requests       ; The call requests that have been finished
	opening-time))      ; The current open state of the doors. Decrements until zero and then continues servicing floors.
```

### Methods

Our solution exploits the fact that all elevators are connected to the same router. This allows us to broadcast.
Broadcasting makes other elevators aware of existence. If an elevator has stopped broadcasting, the other elevators
will detect this by not resetting the time to live counter for that elevator. For every frame where no packet was received
from an elevator it's "time to live" value drops by one, and so, if the time to live reached a limit, the elevator
is forgotten by the one holding the count.

In addition to broadcasting and receiving, we have serialization with an added sha256 check on a specific structure
so that stray packets sent to our ports get ignored. We use Racket's built-in fast-load serialization (FASL)
to serialize arbitrary prefabricated (#:prefab) data and primitive types.

Message sizes don't get larger than about 1200 bytes. This fits well within the UDP size limits.

That pertains the entire networking module.

During each iteration of state, the system collects data from the network, the floor detectors, and the buttons.
It then uses all this data and its previous state to compute where all elevators will be heading next.

This computation is done by each elevator, and serves as a method to make all elevators compute the same
directions with the same knowledge. This allows elevators to be certain that their choice of floor is not already
taken by another elevator.

If there is state lag due to the broadcast/receive latency, then everything is still fine.
The computation (when all elevators have updated all states) will converge on a single best-fit
elevator. So if one lags behind, that elevator may compute wrongly; say it decides it should not
service (request up 4). All other elevators think that the one lagging behind should take it.
During the next iteration. Because nobody has taken the task (because everyone thought the lagging
elevator was taking it), the lagging elevator may get an update that another elevator is no longer eligible
for (request up 4), so this elevator is able to acquire the request.

The elevators sleep before receiving, making it highly unlikely that an elevator lags behind.
The sending is threaded and runs without interruptions (short sleeps only). These sleeps are
more than two times shorter than the main sleep. This gives us a lot of confidence that a packet
has arrived after main's sleep.

## Installation

1. Install Racket https://racket-lang.org/download/
2. In a terminal: `raco pkg install --skip-installed lens libuuid reloadable sha threading`
3. `git clone this-repository`

## Usage

1. Run: `./main.rkt` or `racket main.rkt`

This is all that is needed to run the elevator control software

The software supports hot-swappable code, so you can edit any rkt file and save it.
Edit any racket file using `trce`, `dbug`, `info`, `warn`, `erro`, `crit`, or `ftal` (or any `trce*`, `dbug*`,... derivatives).
Save and observe the log statements.

Generated files will be stored in temporaries/.

## Contributors

* [BourgondAries](https://github.com/BourgondAries)
* [dagerikhl](https://github.com/dagerikhl)

## License

GPL-3.0
