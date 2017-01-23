1:
	What if the software controlling one of the elvators suddeny crashes?
		Other elevators will pick up the work
	What if it doesn't crash, but hangs?
		Other elevators will pick up the work
	What if a message between machines is lost?
		Then the elevator will assume that the peer is dead, and both will pick up the work
	What if the network cable is suddenly disconnected? Then re-connected?
		The elevators both perform the same tasks
	What if a user of the system is being a troll?
		The elevators will just shuffle around
	What if the elevator car never arrives at its destination?
		Another elevator will need to observe this and fix it
2:
	We use git with github as our remote.
3:
	What is concurrency? What is parallelism? What's the difference?
		Concurrency is unordered operations
		Parallelism is running those operations potentially simultaneously

	Why have machines become increasingly multicore in the past decade?
		Because of the heat barrier making it difficult to further improve single cores

	What kinds of problems motivates the need for concurrent execution? (Or phrased differently: What problems do concurrency help in solving?)
		Problems that may fail (and thus fail to yield control). This ensures that not the entire system is brought down.

	Does creating concurrent programs make the programmer's life easier? Harder? Maybe both? (Come back to this after you have worked on part 4 of this exercise)
		Depends on the situation. Control flow is harder to reason about, but functional programming
		can solve this.
		It makes distributed problems easier to handle, but it does create more work.

	What are the differences between processes, threads, green threads, and coroutines?
		A process is a thread that has its own exclusive memory space
		Threads are processes in the same memory space
		Green threads are threads that are scheduled by a library, not the OS
		Coroutines are procedures that implement continuations

	Which one of these do pthread_create() (C/POSIX), threading.Thread() (Python), go (Go) create?
		pthread_create() makes a thread
		threading.Thread() makes a thread
		go creates a goroutine (coroutine)

	How does pythons Global Interpreter Lock (GIL) influence the way a python Thread behaves?
		It makes "threads" concurrent but not parallel

	With this in mind: What is the workaround for the GIL (Hint: it's another module)?
		Create a new python process that communicates via sockets

5:
	Done
