An atomic operation?
	An operation of which an intermediate state can not be observed by outsiders
A semaphore?
	A variable that manages access to data from several concurrent locations by atomic changes and then waiting until a condition has been satisfied
A mutex?
	A mutex is a lock, and can be seen as a binary semaphore
A critical section?
	A critical section is a mutex that is not shared between processes, this allows the critical section to avoid calling the kernel, making for a low overhead lock.
