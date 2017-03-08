# Postmortem: Moloch Asmodeus Rex #

Group 73 was founded in january of 2017 as a working group consisting of [dagerikhl](https://github.com/dagerikhl) and [BourgondAries](https://github.com/BourgondAries).
The goal of the group was to implement elevator control software that is fault-tolerant. Each elevator is
connected to a machine that is connected to the same network. We were to implement the driving software
such that all elevators would work together as well as preventing faults from occurring.

We decided to use the Racket language. It's a mostly functional lisp. dagerikhl never touched lisp.
bourgondaries had some experience with it. We started work early and worked consistently over the course
of two months. Working from home as well as coming together once or twice per week.

In early march that same year we completed our work. We wrote a little documentation and cleaned up the
code. We then hoped that the FAT (Factory Acceptance Test) would turn out well.

## What went right ##

### 1. Exploiting Macros ###

We started with macros very early. A macro in a lisp is an arbitrary syntax transformer. This allowed us to
save a lot of typing. We built a very useful module called "logger". This module contains the macros
`trce`, `dbug`, `info`, `warn`, `erro`, `crit`, and `ftal`. Each of these are equivalent except that they
write a different symbol out. For example: `(trce (+ 1 2 3))` writes `(trce: (+ 1 2 3) = 6)`.
Because of its versatility we were able to easily add print/debug statements anywhere which both
indicated the expression being printed as well as its evaluation. Very useful.

We ended up defining the same macros again but with stars at the end of the names: `trce*`, ... and so on.
These macros would not print the expression they were given, only the result got printed.
This turned out very useful inside the `~>` threading macro.

The nice thing is that we defined all the above macros using a macro.
The entire logger generating macro at the time of writing is 13 lines long.

### 2. Threading (Pipe) Macros ###

Our implementation uses threading macros `~>`. This isn't related to multi-threading, it can instead
be thought of as a sort of pipe that moves data from the first to the second expression.

Inside `core.rkt` we created a pipe of single functions like `(~> data func1 func2 func3 ...)`. Data would pass
through `func1`, which returns new, transformed data. That transformed date is input to `func2`, and so on.
We kept this pipeline mostly free of expressions, and moved all the functions associated with it into
`utilities.rkt`. This turned out to give us very readable code.

In the future we'd construct our program with the pipe as the most important part. We'd clean up core
and remove the non-pipe code and place it somewhere else.


### 3. Functional Programming ###

Side effects are kept to an absolute minimum. This makes the code more understandable and debuggable.
In fact, one bug that surfaced was completely captured by simply printing the current state (which is
an explicit variable). We then set the initial state to that buggy state and observed where the bug would
occur. Paired with `trce` we were able to find the bug very quickly.

The code never uses `set!` mutation directly. The only place we use mutation is in `udp-bind!`, because
there's no other way to to UDP in native racket.

### 4. Data Oriented Design ###

We were surprised by the effectiveness of designing great data structures up front. Why did we do this? Because
Linus Torvalds inspired us:

> I will, in fact, claim that the difference between a bad programmer and a good one is whether he considers his code or his data structures more important.

Recognizing this we designed the state of the program up front. We got almost everything right. What we removed was
the `resting-floor` value since we ended up not implementing this. This would move the elevator to a floor if there
were no more requests.

The code is built around the `state` data structure. It sends part of this over the network, merges it, transforms it,
reads it, and uses it to set lamps, set the elevator motor, and so on. The state is directly used by the algorithms
for computing the optimal request for an elevator.

We eventually needed some more data (for avoiding spammy key states) so we simply built a data structure containing
the `state` structure in addition to a few other fields. This allowed us to keep our core algorithms exactly the same.


### 5. Simple Networking ###

The solution we opted for ended up being very simple logic-wise. It just broadcasts the current elevator's state at
a rate that's above the nyquist frequency of all other polling elevators. This pretty much guarantees that all
elevators will have all states simultaneously. Even if a packet is lost, state is remembered (for a certain period).
So each elevator computes the exact same paths for all elevators.

Because of this simple model we were able to just broadcast and receive and nothing more. The built-in serialization
especially helped us and we checked the incoming packets for validity.

We had no need for a master-slave implementation or anything more complicated than simple broadcasts.
This allowed us to focus on the core logic of computing good paths and fault tolerance.

### 6. Self-Organization and Team Dynamics ###

We organized our tasks on github using issues and discussed them over messenger. Our work is mostly self-driven
and we were consistent and disciplined. Using a software development framework like scrum would add cognitive overload
and stress instead of giving us the freedoms to experiment and play with our code because it sets deadlines. But it may be my own biases against methodologies talking here.

We had a final deadline from the class but it didn't really affect us much. We finished ahead of time
and under budget.

The team was lighthearted and fun. We never had any personal conflicts and got along well. I think what
motivated us quite a bit is that we'd deride the other teams for their language choice. This made us very competitive.
We enjoyed the linguistic chauvinism and derision, but we were able to shift gears into serious mode quickly if needed.

## What went wrong ###

### 1. Managing Knowledge / Entry Barrier ###

The team was not very acquainted with racket, nor lisp, nor functional programming much. This caused some initial
friction in our work. We decided to delegate work to different modules so we could work mostly independently.
We also pair-programmed quite a bit, however, functional programming definitely required a shift in the team's
previous programming mentality and caused some slip-ups in the beginning.

Next time we should share resources about functional programming and racket, and delegate more strictly so
that everyone can explore on their own instead of having the cognitive overload of reading somebody else's code.

### 2. Testing and Units ###

We didn't test enough. There is a tests directory that contains some tests but it seemed so tedious to write.
Secondly, our tests wouldn't be perfectly pure since everything depended on `id` and `name`, which were generated
randomly or loaded from a file. Our attempts at utilizing first-class modules (Units) failed as we dissatisfied
with the results.

The tests also became big for algorithms that would take entire-states and computed something from them.
We should have broken up these functions into two parts:

1. Extract the necessary data
2. Operate on the necessary data

Take `compute-available-call-requests` for example, this takes in `elevators`, which is a hash-table. We could
instead separate extraction of `*-requests` (which lies inside `elevators`) and operation on that data. This would make testing easier.

Next time we must separate functions into extraction and operation and test both. Testing the sum will be unnecessary.

### 3. Minimizing Global State ###

There are some threads that are running in the background and they are defined globally (although they are confined
to their respective modules). This became a little bit of a problem when reloading a file. The thread would still
be active and reloading re-initializes the file which creates an entirely new thread. Luckily this only happens when reloading the specific files containing the thread, which was very rarely done.

We should've put all state inside the core loop variable so that it can be handled explicitly, even though this breaks
the idea of separation of concerns. It's a small annoyance but an annoyance nonetheless.

We also have a global `id` and `name` which are used to identify the elevator. It's global because it's used
by lenses, which are used almost everywhere in utilities. We tried removing its globality with units but we failed.

On a positive note, all our global values (except the udp port) are immutable. We don't change them. Ever.

Next time we should model all state in the core loop of the program as explicit variables (or inside a structure). All functions ought to be re-entrant.

### 4. Finding Conventions ###

We spent some time discussing coding conventions without any real style guide.

Next time we ought to find a style guide and stick to it so we can avoid bikeshedding.

### 5. Generating Live Documenation ###

We should have generated live documentation like one normally does in Rust. This means you comment your code
with code samples that are run whilst generating documentation in an HTML format. The documentation thus never
expires because it's checked when generated. We could have used something like Scribble (specific to Racket)
in order to generate documenation of our APIs and implementation. This would make it easier to read about
a module and its interface.

Next time we should make it easy to add comments that get turned into documentation using some tool.

## Conclusion ##

I can't possibly enumerate all the little things we did right and wrong during this project but I think we definitely
had a great team learned a lot from it. We've definitely leveled up as programmers.

We're proud of our work, which overall gave us many mental challenges to overcome. The solution has simple
fault tolerance methods (even a monitor for restarting the program) in only about 634 lines of code.
Normal projects for this class range from 500-1500 LoC.

If you've never explored a lisp (like Racket) then I urge you to do so for the profound enlightenment it bestows upon you once you "get it".
I'd like to finish this part with an excerpt from Let Over Lambda:

> Macros are the single greatest advantage that lisp has as a programming language and the single greatest advantage of any programming language. With them you can do things that you simply cannot do in other languages. Because macros can be used to transform lisp into other programming languages and back, programmers who gain experience with them discover that all other languages are just skins on top of lisp. This is the big deal. Lisp is special because programming with it is actually programing at a higher level. Where most languages invent and enforce syntactic and semantic rules, lisp is general and malleable. With lisp, you make the rules.

- Let Over Lambda, ch. 1
