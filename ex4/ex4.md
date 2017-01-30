#### Will you use TCP, UDP, both, or something completely different?
We will be using UDP.

#### In the case of a master-slave configuration: Do you have only one program, or two?
We do not have a master-slave configuration.

##### Is a slave becoming a master a part of the network module?
Irrelevant.

#### If you are using TCP: How do you know who connects to who?
We are not using TCP.

##### Do you need an initialization phase to set up all the connections?
Irrelevant.

#### Will you be using blocking sockets & many threads, or nonblocking sockets and select()?
We will be using 1 thread with 1 blocking socket that handles all received UDP-messages.

#### Do you want to build the necessary reliability into the module, or handle that at a higher level?
It will be handled by another module, the main-module. Not at a higher level, but in a different part of the system.

#### How will you pack and unpack (serialize) data?
We let Rackets Fast-Load Serialization library handle all of our serialization.
We only need to call a function to serialize the data, and another to de-serialize it.

##### Do you use structs, classes, tuples, lists, ...?
We use lists, and possibly structs in the future.

##### JSON, XML, or just plain strings?
We serialize to byte-strings.

##### Is serialization a part of the network module?
Yes.
