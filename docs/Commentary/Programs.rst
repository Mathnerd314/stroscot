Exemplary programs
##################

A programming language is no good unless some significant programs have been written in it. This is mainly so that dark corners of the language have been explored and debugged, but there is also a "halo effect" of "I like program X, therefore I like programming language L that X is written in". Also, every popular PL solves a specific set of programming problems really well, in specific domains. So get the set of problems right, and the design for the PL will reveal itself naturally. Or solve all the programming problems and win the game. Once Stroscot gets users it can switch to problem-driven design: work with real-world users who have problems, abstract and implement solutions for them.

Social network
--------------

A social network is a large program for any web development / app development framework to cut its teeth on. For example, the Hack programming language was closely associated with Facebook.

Basic features (a la Reddit clone)
* creating a post
* replying - can reply to multiple posts at once, creates its own post - nested view, falls back to linear view when too deep
* reacting to a post (emoji, save, remove - hide/report)
* ranking a post (adjusting rating to be between two other posts)

Accounts:
* invite system, one account per person
* "shells", multiple usernames per account. Some way to share shells between accounts.

As far as theme, Goodreads has received a lot of criticism and there might be room to replace it.

High performance
----------------

High-performance programming in a systems programming language aims to get the most out of hardware. It tackles large problems on (relatively) slow hardware, such as wind tunnel simulations. Small efficiencies add up.

Necessities:
* ahead-of-time compilation to machine code (bare metal)
* multithreaded execution. (Spawning new processes does not count.)
* composable (assembly is simply not practical)
* some amount of users / libraries

Features:
* call C functions with minimal overhead
* manual memory management (reference counting/GC has overhead)
* good benchmarking/profiling tools
* SIMD instructions readily available
* standard, cross-platform way to build programs, run tests and add dependencies.

Server
------

A benchmark that people seem to pay attention to is network performance, writing web servers to handle as many clients as possible simultaneously.

Choices:
blocking I/O and one client per OS thread (read/write) - requires stack for each thread, memory exhaustion
blocking I/O with userspace fibers (M:N threading model) - complex. A M:N scheduler must have a userspace component. This doubles the icache/dcache footprint. Fairness, nice, SMP balancing, memory allocation, TLS, RT scheduling, preemption, tid, debugging, security in userspace is hard - little control and only indirect access to statistics. Kernel events must trigger upcalls to activate context switches in the userspace component, but e.g. for memory there is no way to do async and a kernel context is blocked. M:N makes userspace<->userspace context switching faster, but slows down kernel<->userspace interaction. And kernel interaction is by default cheap on modern systems. For RT scheduling you would have to either set the priority every context switch, eliminating any performance advantage, or fall back to 1:1 scheduling for those threads.

OTOH as Go shows (Go HTTP servers are reasonably fast), you can have cooperative coroutines with tiny userspace stacks. You start with one thread per processor (possibly bound so there's no CPU shuffling?) and have this pool of homogeneous threads run through a task queue. This does no context switching of any kind, the only overhead is that the queue is concurrent (Go has per-thread queues too to mitigate this).

nonblocking I/O and level-triggered readiness notification (select/poll/kqueue) - requires fd for each connection
nonblocking I/O and readiness change notification (kqueue, epoll, realtime signals)
asynchronous I/O and completion notification (AIO, io_uring, IOCP)
server code in kernel (kttpd, TUX Threaded linUX webserver)
Bring the TCP stack into userspace - netmap, Sandstorm
Windows UMS - one thread can switch execution to another without stopping (discussed `here <https://www.youtube.com/watch?v=KXuZi9aeGTw>`__).

CDCL Prolog
-----------

Just a fun experiment, what would Prolog look like if you replaced its backtracking SLD resolution algorithm with the algorithm from a modern SAT solver? CDCL (Conflict-driven clause learning) is the newest and fastest algorithm and seems like the one to use.

Other ideas
-----------

Trading engine (Stock, options, cryptocurrency...)
Game engine (based on O3DE, Godot, or Unreal)
UI toolkit
Raytracer
Vector graphics library (bonus points for supporting SVG drawings and OpenType fonts)
abstract assembly (like High Level Assembly)
expression problem (compiler)
inventory tracker for android (like todo-mvc but better)