Exercise 10 : FSP
=======================


Introduction
----------

A lot of relevant information can be found around this [web page](http://www.doc.ic.ac.uk/~jnm/book/).

An explanation of the FSP notation can be found [here](http://www.doc.ic.ac.uk/~jnm/LTSdocumention/FSP-notation.html).


**Installing LTSA**

 1. Download LTSAtool.zip from the “Files for Exercises” folder
 2. Extract the files, for instance to Desktop/
 3. Find “LTSA.jar”, right click on it, and choose “Open with ‘Sun Java 6 Runtime’ ” or install directly from a shell with the following commands:

	- cd ∼/Desktop
	- unzip LTSAtool.zip
	- java -jar ltsatool/ltsa.jar

Assignments
-------------------------

**Introductory Assignment** 

Read through reference manuals and experiment. You can find examples
in “FSPEksempel.doc”. Even more examples can be found under the File
Examples menu in LTSA.

**Safety/Deadlock Assignment:**

The absence of deadlocks is a built-in safety criterion in LTSA.
Create a system with a deadlock. Does LTSA detect it?

It is easy to see from the state model that we have a deadlock, because there
will be a state in the diagram that there is no way to get out from. How can
you detect a deadlock from the FSP model itself?

**Dining philosophers**

See [http://en.wikipedia.org/wiki/Dining_philosophers](http://en.wikipedia.org/wiki/Dining_philosophers) to refresh your
memory of the problem.

The code for a "dining philosophers" system can look like this if we
model forks with semaphores:

    phil1(){
      wait(fork1)
      wait(fork2)
      // eat
      signal(fork2)
      signal(fork1)
    }

**Assignment:** 

Draw the transition diagram (manually) for a system whith
two philisophers.

**Assignment:** 

Model a system with 3 philosophers and 3 forks in FSP and
demonstrate a deadlock. (Skip this assignment if you choose to do the next one)

**Optional Assignment:** 

Extend the FSM description to handle N
philosophers by using indexing, prefixing, relabeling etc. How many
philosophers can LTSA handle?  Be a little careful, trying a very
large number can make the computer rather unresponsive.

**Optional Assignment:** 

We want to get rid of the deadlock. Make one of
the philosophers left-handed (picking up the forks in the other
order). Does that solve the problem?

**Optional Assignment:** 

The left-handed philosopher has consequences for
fairness. Why? Can you come up with (or Google..) fair solutions that
does not have a deadlock?

Communication
-------------------------
**Assignment:**

These two threads communicate using synchroneous communication:

    t1(){
      while(1){
        send(channel1,data)
        recv(channel2,data)
      }
    }

    t1(){
      while(1){
        recv(channel1,data)
        send(channel2,data)
      }
    }

How would you model the sending of data over a synchroneous channel in FSP? How many states will there be in the transition diagram of this system?

**Assignment:**

Assume the channels are buffered (asynchroneous
communication). How would now a channel need to be modeled in FSP.


