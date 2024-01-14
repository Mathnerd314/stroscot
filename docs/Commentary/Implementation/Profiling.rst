Profiling
#########

The goal of the profiler is to measure performance, and allow making optimization decisions based on that.

Code performance is unpredictable; even simple programs may be optimized in ways that the reader or writer does not expect. Performance often depends on undocumented and obscure details of the program's environment, making background knowledge or heuristics and guidelines useless. Therefore, profiling must be available from the first time the program is run so as to establish baseline performance.

Resources
=========

We model a computer using a "functional block diagram", i.e. blocks and buses. The level of detail varies depending on the application. Buses typically have sufficient capacity for all data so can be trivialized, but for CPU internals, they are a bottleneck, so are left in. Similarly PSU and cooling are not usually a problem, but I left them in.

Blocks:

* processor - time, caches, I/O bandwidth

  * 8x die - 4x DDR, 8x I/O, L3 cache

    * 8x core

      * caches - L1I, L1D, L2 I+D, L0/L1/L2 TLB and BTB, L1 Perceptron, L2 TAGE, operation, return stack, indirect target array, micro-tags, microcode ROM
      * decode, dispatch

  * I/O controller die - memory, PCIe, hub, secure processor

* graphics card
* memory

  * physical memory used
  * virtual memory allocated/committed
  * virtual memory reserved (windows)
  * private bytes - allocated to process, shared bytes - includes memory-mapped files, shared DLLs
  * working set - set of pages accessed within a given time period (near future, near past)
  * resident set - program memory current in physical memory
  * `profiler <https://blog.mozilla.org/jseward/2011/01/27/profiling-the-browsers-virtual-memory-behaviour/>`__
  * reported on Linux in /proc/<PID>/status - split: heap, stack, executable code, loaded DLLs

* motherboard
* storage
* power supply - typically left out
* case/cooling - also typically left out

::

  branches (& misses)
  branch-instructions
  cache-references (& misses)
  L1-dcache/icache prefetches (&misses)
  LLC-stores
  loads: LLC, dTLB, iTLB, node, L1-dcache/icache (&misses)
  faults: alignment, emulation, major, minor, page
  bpf-output
  cgroup-switches
  context-switches OR cs
  system calls
  cpu-migrations OR migrations
  dummy

  cpu-cycles
  stalled(idle)-cycle backend/frontend

  instructions
  cpu-clock
  task-clock
  duration_time
  user_time
  system_time


  branch prediction -> {L1l cache, op cache}
  L1l cache -> decode
  decode -> micro-op queue 4 instructions
  decode -> op cache
  op cache -> micro-op queue 8 fused instructions
  micro-op queue -> {integer register rename, floating point register rename} 6 dispatch ops
  integer register rename -> {ALU scheduler x 4, AGU scheduler} -> {physical register file, reorder buffer}
  reorder buffer <-> register rename

  -> {ALU x 4, AGU ld st x 2, AGU st}
  FP register rename -> scheduler -> physical register file -> {FMA/FMUL x2, FADD x2}
  AGU ld st -> load store queues -> L1D cache <-> L2 cache

Metrics
=======

Key metric types:

* throughput: operations per second or similar. E.g. for disks, IOPS is reads and writes per second.
* response time: time for an operation to complete, from start to finish
* latency: time waiting for an operation to complete, generally the largest/only component of response time
* utilization: % time active (non-idle time). Doesn't necessarily mean saturated. E.g. if a CPU core is always running, even if it's a multicore system, the system is at 100% CPU utilization. (which is why fine grained resource utilization stats are much more useful)
* saturation: requested work / capacity. For example, a hard disk may be spinning and have lots of queued requests but still be able to service more, hence utilization 100% but saturation lower than 100%. At 100% saturation the disk cannot perform any more work; if more requests are sent (pressure is applied), the disk's request queue will simply fill up (oversaturation, >100%). Similarly memory usage, at the 100% saturation mark requests start going to swap or OOM. Measure capacity and hence saturation by graphing load vs latency and finding an inflection point, but a sharp point may not exist, so often simply reported as queue length without dividing by capacity.
* exceptions: log message or abnormal return code. Can be quantified as % of requests or count per second. The usual exception broadness applies, like "is a cache miss an exception?" (yes). The code can stop on exception, making it obvious, but more often it automatically recovers from exceptions and requires detailed logging to investigate.

Saturation is the catch-all metric, you can use it for almost all resources and anything at or over 100% is definitely a bottleneck. Of course it is also the hardest to measure. And exceptions are also important, and don't really have a saturation point, just typical ranges.

Statistics
==========

Throughput / utilization / saturation are all instantaneous measurements. Practically they cannot be recorded every cycle, so instead reporting 1-, 5-, and 15-minute averages is typical. But this can disguise short spikes; use better statistics. Also, for CPU utilization, if it's just one core in use, but it hops around, the utilization can be misleading. The balance of CPU utilization among all cores must be considered.

Methods
=======

* counters: most simply, number of times an operation was performed. They start at 0 and increment.
* event log: a list of timestamps. Can just count the events, but also you can calculate average rate, spikes, and other statistics.
* sampling: perform detailed analysis only some of the time, so that performance is not affected as much



* sensitivity: for each resource, when adding a little bit more of that resource, the amount of performance increase divided by the amount added
* scalability: throughput as a function of load
* execution: start time and latency (duration / finish time)



Time can be measured as internal clock, CPU cycles (~0.3 ns),


A flame graph shows a tree of calls annotated with execution times.

Measure

* time and memory usage.
* throughput (calls/second)
* A/B testing of multiple implementations

for functions, expressions, programs, etc. Profiling can be at different granularities (module, function, instruction). Generally, recording at the finest level is most desirable, and similarly automated optimizations mainly operate at the finest level, but it can be useful to produce summary statistics more understandable to humans.

Use statistical sampling and hardware performance counters to avoid overhead. Checkout criterion, papers on LLVM hardware sampling.

Intel VTune, perf, and various profilers available for different programming languages.

Action
======

Once specific "hot paths" for optimization have been identified, the general strategy is to identify inefficiencies, such as:

* unnecessary operations - remove them
* bad data access patterns - reorder operations or use a different algorithm/data structure
* repetitive or redundant computations - cache/memoize operations
* independent computations - take advantage of hardware parallelism, such as SIMD, multiple cores, or the GPU

It is good to profile the program after each change to ensure it is actually an improvement. Static performance models are pretty accurate, so automated optimizations will generally be improvements, but the static models are only so accurate and there are always exceptions.


profilers have overhead - e.g. Python profiler slows down by 2x, making measurements inaccurate.


 grabs a huge amount of memory
 OS drops most of its page cache
 many page misses in rest of program

Caches
======

For caches there are a few basic metrics: putting stuff in the cache, taking it out, and the miss rate. There are many caches: OS page cache, cpu cache, BTB, TLB, GPU texture cache, disk cache, JITted code cache. It is important to attribute miss costs properly, e.g. one piece of "cold" code might put a lot of stuff in and then cause misses in the important "hot" code.

For example, using XXHash made an individual function faster, but in a larger program it was slower than using Python's native hash because Python's hash was already in L1 instruction cache. Similarly, calling a function 100 times, then pass the results to another function, etc. for a total of 8 functions and 800 calls, is better than interleaving the function calls and doing the 8 functions on one item, then on another item, and so on.

Hyperthreading

Benchmarking
============

A good suite of benchmarks include microbenchmarks like optcarrot, small nontrivial programs like the language shootout tasks, and large applications like a production webserver. All of them are useful for detecting regressions.

What is the ideal length of a benchmark? :cite:`suhEMPExecutionTime2017` measured programs of different runtimes - since the tasks were very similar, it is reasonable to try to fit standard deviation / runtime as a function of runtime. Going through the equations in Google Sheets and EMPv5 data (Table XIX), the linear law has R^2=0.84, overestimates all deviations up to 1000s, and barely fits the few long runs. The quadratic has R^2 = 0.931 and fits the data past 4s pretty well but again overestimates the 1,2,4s point. Also it makes little sense as a function, why would the deviation decrease past 15,000 s? The exponential and log functions are terrible. Finally, the power law has R^2 = 0.861. On a log-log graph it has a decent pattern of over/undershoot - it goes exactly through the 1s point, underestimates the next few, over estimates the next few, and finally underestimates the least few. The graph is mostly flat from 10s to 512s, so perhaps the first few results were unusually good and the last were affected by undiscovered sources of large errors. Plotting max-min as a function of duration (again log-log) we see this is the case - 1s is unusually low, then there is a mostly linear patch up to 1000s, then 2000-16,000s jumps sharply upwards. Again the power series is the best fit, R=0.817, by a long shot, with the next highest being the quadratic with R^2=0.572. Limiting just to 4s-1024s the fit is improved and the residuals look random for a power fit to max-min. For the standard deviation, excluding the outliers, a linear fit actually looks better than a power law. So we conclude that an ideal benchmark is 4s-1024s. And practically, shorter is better, so aiming for 4-8s is probably best. In particular the standard deviation is 23.634 t^0.505 without noise mitigation and 0.589241 t^0.342 with noise mitigation, where t is the runtime in seconds.

There are several metrics:

* clock cycles, as measured by the rdtsc machine instruction or logic analyzer
* wall clock time, as measured by stopwatch, the time command, or Java’s System.currentTimeMillis()
* machine code size
* one of the above for a specific program part, such as a method (as measured by profiling or sampling)

Different methods have different resolution (reporting capability), granularity (division of report into program parts), precision (variation betwen runs), and accuracy (closeness to "actual value", a difficult concept to define in benchmarking).

Many variables affect precision and add noise. Often, we can control the variable to a fixed value, and remove its variance. But this may mean our results are unrealistic; for example frequency scaling is not usually turned off. A more clever approach is to measure the variable or proxies for the variable, and characterize the runtime as a function of the variable. This does generally require more benchmarking runs in order to gather the necessary statistical power. And even with many variables measured or controlled, there will still be noise. Still, it brings up what we want to measure: average? minimum? some theoretical ideal?

In the initial stages of optimization, no statistical profiling is needed as 10%+ speedups are obvious. But as optimizations become more complex, speedups become smaller and powerful statistical techniques are of great help. Generally the task must be at least 5-10x larger than the measurement precision to get useful results without statistics. Thus, if the task is 10 msec, then the measurement technique must have better than 1 to 2 msec precision. This is only a rule of thumb; more precision is always better, but less precise measurements can be compensated for by doing more of them and analyzing statistics.


Variables include:

* CPU frequency and voltage scaling (proxy: performance counters :cite:`snowdonAccurateRuntimePrediction2007`)
* execution of other programs/daemons (load)
* CPU affinity
* OS version
* CPU temperature
* filesystem cache, swap usage
* processor-specific optimizations
* Environment block contents. It contains strings which are generally consistent on one system but vary across systems, such as the username. It can lead to certain locations being aligned or misaligned, causing significant variations at run-time. It is mainly the size of the env block that matters.
* ASLR - can be disabled globally or per-process basis with ``setarch -R``. But e.g. command-line arguments affect memory layout even when ASLR is disabled. Also the seed cannot be controlled. Most people run with ASLR enabled.


* stabilizer - this randomizes memory layout, allowing the measurement of a layout-independent performance. Of course, if one is optimizing the memory layout then randomization is counterproductive.
* `nanoBench <https://github.com/andreas-abel/nanoBench>`__ - this has a kernel module to allow reading hardware performance counters for microbenchmarks without incurring much overhead
* `benchExec <https://github.com/sosy-lab/benchexec>`__ - this is very similar to the tool I remember, but focused on limiting resources rather than getting precise measurements
* `hyperfine <https://github.com/sharkdp/hyperfine>`__ - this does some basic warmup and statistics, useful as a baseline for the bare minimum of Benchmarking
* `temci <https://github.com/parttimenerd/temci>`__ sets up an environment

profiling:

* generate approximate profile of new/modified code, guessing using heuristics
* cold functions, functions executed once, loopless functions, min/max/average number of loop iterations, branch probabilities, values of expressions in program, order of first execution of functions
* AutoFDO profile https://perf.wiki.kernel.org/

