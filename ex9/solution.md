Exercise 9 : Scheduling
=======================


Properties
----------
**Task 1:**
 1. Some things are more "important": kernel stuff (like device drivers), real-time constraints, and - for "general-purpose" operating systems (like Windows) - being responsive to user interactions.   
    For message-passing/process-oriented systems, we often create processes for other reasons than "one thread per timing demand", so assigning priorities in any meaningful way may be difficult.
 2. The scheduler must be predictable, so that we can analyze the system. We want the analysis to be simple, but not too conservative, as we want to facilitate high utilization. The result of the analysis must show that all tasks meet their deadlines.
 


Inversion and inheritance
-------------------------

**Task 2:**
 1.   
| Task\Time | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12| 13| 14|
|-----------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|  a        |   |   |   |   |`E`|   |   |   |   |   |   |`Q`|`V`|`E`|   |
|  b        |   |   |`E`|`V`|   |`V`|`E`|`E`|`E`|   |   |   |   |   |   |
|  c        |`E`|`Q`|   |   |   |   |   |   |   |`Q`|`Q`|   |   |   |`E`|
 
 
 2.  
| Task\Time | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12| 13| 14|
|-----------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|  a        |   |   |   |   |`E`|   |   |`Q`|   |`V`|`E`|   |   |   |   |
|  b        |   |   |`E`|`V`|   |   |   |   |`V`|   |   |`E`|`E`|`E`|   |
|  c        |`E`|`Q`|   |   |   |`Q`|`Q`|   |   |   |   |   |   |   |`E`|

**Task 3:**
 1. Priority inversion: That a high-priority task ends up waiting for a lower-priority task, which can happen if they share a resource.  
    Unbounded priority inversion: That the high-priority task may potentially end up waiting forever. The unboundedness occurs when there are one or more medium-priority tasks that prevent the low-priority task from running - and thereby releasing the shared resource.
 3. Priority inheritance does not prevent deadlocks:  
    Consider the simple deadlock example:  
    Task a: `lock q, lock v, unlock v, unlock q`  
    Task b: `lock v, lock q, unlock q, unlock v`  
    When both tasks lock one resource each, it is too late to boost priorities of either one. Priority ceiling protocols - however - *do* prevent deadlock.

Utilization and response time
-----------------------------

**Task 4:**
 1. 
   - Fixed set of tasks (No sporadic tasks. Not optimal, but can be worked around)
   - Periodic tasks with known periods (Realistic in many systems)
   - The tasks are independent (Completely realistic in an embedded system)
   - Overheads, switching times can be ignored (Depends)
   - Deadline == Period (Inflexible, but fair enough)
   - Fixed Worst Case Execution Time. (Not realistic to know a tight (not overly conservative) estimate here)
   - And in addition: Rate-Monotonic Priority ordering (Our choice, so ok)
 2. `U = 5/20 + 10/30 + 15/50 = 0.8833`, `3*(2^(1/3)-1) = 0.7798`  
    Conclusion: Utilization test fails, task set may or may not be schedulable
 3. 
   - Task `c`:  
     `w0 = 5`
     => `Rc = 5 <= 20`, ok
   - Task `b`:  
     `w0 = 10`  
     `w1 = 10 + ceil(10/20)*5 = 15`  
     `w2 = 10 + ceil(15/20)*5 = 15`  
     => `Rb = 15 <= 30`, ok
   - Task `a`:  
     `w0 = 15`  
     `w1 = 15 + ceil(15/30)*10 + ceil(15/20)*5 = 15 + 10 + 5 = 30`  
     `w2 = 15 + ceil(30/30)*10 + ceil(30/20)*5 = 15 + 10 + 10 = 35`  
     `w3 = 15 + ceil(35/30)*10 + ceil(35/20)*5 = 15 + 20 + 10 = 45`  
     `w4 = 15 + ceil(45/30)*10 + ceil(45/20)*5 = 15 + 20 + 15 = 50`  
     `w5 = 15 + ceil(50/30)*10 + ceil(50/20)*5 = 15 + 20 + 15 = 50`  
     => `Ra = 50 <= 50`, ok
     
    Conclusion: Task set is schedulable  

    The utilization test is sufficient, but not necessary. The response-time analysis is both sufficient and necessary.
    
 4.  
| Task\Time | 0 | 5 | 10| 15| 20| 25| 30| 35| 40| 45| 50| 55| 60| 65| 70| 75| 80| 85| 90| 95|100|105|110|115|120|125|130|135|140|145|
|-----------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|  a        |   |   |   |`a`|   |`a`|   |   |   |`a`|`a`|`a`|   |   |   |`a`|   |   |   |   |   |`a`|`a`|`a`|   |   |   |   |   |   |
|  b        |   |`b`|`b`|   |   |   |`b`|`b`|   |   |   |   |   |`b`|`b`|   |   |   |`b`|`b`|   |   |   |   |   |`b`|`b`|   |   |   |
|  c        |`c`|   |   |   |`c`|   |   |   |`c`|   |   |   |`c`|   |   |   |`c`|   |   |   |`c`|   |   |   |`c`|   |   |   |`c`|   |
 
