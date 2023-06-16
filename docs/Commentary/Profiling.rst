Profiling
=========

The goal of the profiler is to measure performance, and allow making optimization decisions based on that.

Resources
=========

We model a computer using a "functional block diagram", i.e. blocks and buses. The level of detail varies depending on the application. Buses typically have sufficient capacity for all data so can be trivialized, but for CPU internals, they are a bottleneck, so are left in. Similarly PSU and cooling are not usually a problem, but I left them in.

Blocks:

* processor

  * 8x die - 4x DDR, 8x I/O, L3 cache

    * 8x core

      * caches - L1I, L1D, L2 I+D, L0/L1/L2 TLB and BTB, L1 Perceptron, L2 TAGE, operation, return stack, indirect target array, micro-tags, microcode ROM
      * decode, dispatch

  * I/O controller die - memory, PCIe, hub, secure processor

* graphics card
* memory
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
