Exercise 5 : Call for transport
===============================

The core C driver [is available here](https://github.com/TTK4145/Project/tree/master/driver).

 - `io.c` and `io.h` define the core functions needed to read from and write to the libComedi device
 - `channels.h` gives readable names to the subdevices/channels used with the elevator
 - `elev.c` and `elev.h` show one way of turning `io` and `channels` into a higher level abstraction
 - `main.c` is an incomplete test program to verify the functionality of the `elev` API.


###1: Creating a driver
Interface to the C code, and create a driver for the elevator in the language you are doing the project in.

 - You may want to avoid using the `elev` files:
   - They implement an abstraction you may not agree with.
   - They also define an enum and two arrays, which are not necessarily portable.
   - This means you will also need to convert `channels.h` to the language you are using. You do not need to convert the `PORT` definitions, however.
 - The functionality for Stop and Obstruction can be ignored in the project. Whether or not you want to include them (for the sake of completeness) is up to you.
 - You will need to do polling at some level. Consider if you want to include this as part of the driver.
   - If you include it, you should only notify the "owner" of the driver instance when an event happens (eg. a button is pushed, a floor is reached, etc).
   - The owner should preferably state which events it is listening to. Unused events should at the very least not leak resources.

####Note for Go:
When you create `io.go` (or equivalent) to wrap the C functions, you need to include this at the top of your file:
    
```go
package driver  // where "driver" is the folder that contains io.go, io.c, io.h, channels.go, channels.h and driver.go
/*
#cgo CFLAGS: -std=c11
#cgo LDFLAGS: -lcomedi -lm
#include "io.h"
*/
import "C"
```

Even though it looks like a multiline comment, it is actually evaluated! See [the `cgo` command](http://golang.org/cmd/cgo/).

####Note for Rust:
Interfacing C code in Rust is done through a Foreign Function Interface (FFI for short). This is explained in [the official Rust documentation](https://doc.rust-lang.org/book/ffi.html). 

To quickly summarize the page, Rust can call C functions from an already existing C library. This means one of two things: either interface directly to libcomedi and create your own IO and Elev driver, or create a static library of the supplied C driver files and create a driver built on existing IO and/or Elev interface. 

To create a static library from the existing C files, a build script can be used. See [here how this is set up](http://doc.crates.io/build-script.html). Remember that the name of the library **must** start with "lib" and end with ".a". Example "libelev.a". 

This how linking and defining the C functions to be called in your Rust code:
```rust
// Name of library is either comedi or the name of the static library.
// If the static library is called libelev.a, then (name = "elev").
#[link(name = "NAME OF LIBRARY")]
extern {
    // All function prototypes to be used from the specified library, in Rust syntax of course.
}
```
   
###2: Testing your driver
Write a test program to make sure that the driver works as expected.

 - You should test all the functions you have created.
 - You should test functions with invalid arguments, and other corner cases.
 
 
###3: Using the simulator

(This is entirely optional)

The simulator [is available here](https://github.com/TTK4145/Project/tree/master/simulator).

Extend your driver (and build scripts) to also work with the simulator.



