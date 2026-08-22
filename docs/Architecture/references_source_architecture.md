Your `sources.md` file covers most parts of your issue tree; below is a best‑effort mapping from major source clusters to specific components, plus a note on sources that are largely orthogonal.

***

## Layer 0 – Language Semantics / Semantic Foundation

### 0.1 Native Dispatch Runtime

**0.1 Native Dispatch Runtime #7**

Sources about real ISAs, microarchitecture, and low‑level execution behavior:

- architecture support and popularity
    - https://www.quora.com/What-kind-of-instruction-set-architecture-do-modern-processors-use
    - https://en.wikipedia.org/wiki/Microarchitecture
    - https://www.cpubenchmark.net/share30.html
    - https://benchmarks.ul.com/compare/best-cpus?amount=0&sortBy=POPULARITY&reverseOrder=true&types=MOBILE,DESKTOP&minRating=0
    - https://cpu.userbenchmark.com/
- x86 ISA and microarchitecture:
    - https://software.intel.com/content/www/us/en/develop/articles/intel-sdm.html, https://developer.amd.com/resources/developer-guides-manuals/
    - https://github.com/google/EXEgesis, llvm-exegesis
    - https://github.com/xoreaxeaxeax/sandsifter, https://blog.can.ac/2021/03/22/speculating-x86-64-isa-with-one-weird-trick/, https://raw.githubusercontent.com/chip-red-pill/udbgInstr/main/paper/undocumented_x86_insts_for_uarch_control.pdf
    - opcode tables
        - https://intelxed.github.io/ https://github.com/intelxed/xed/blob/main/datafiles/xed-isa.txt
        - https://github.com/HJLebbink/x86doc/tree/master/Python
        - https://github.com/netwide-assembler/nasm/blob/master/x86/insns.dat
        - https://sourceware.org/git/?p=binutils-gdb.git;a=blob;f=opcodes/i386-opc.tbl;h=b0530e5fb82f4f4cd85d67f7ebf6ce6ebf9b45b5;hb=HEAD
        - https://github.com/icedland/iced/blob/65d1f49584247a09dcc2559727936a53014268f5/src/csharp/Intel/Generator/Tables/InstructionDefs.txt
        - https://github.com/RRZE-HPC/OSACA/tree/master/osaca/data/isa
        - https://github.com/NationalSecurityAgency/ghidra/blob/master/Ghidra/Processors/x86/data/languages/ia.sinc#L1594
        - https://cs.opensource.google/go/go/+/master:src/cmd/internal/obj/x86/avx_optabs.go;l=1791?q=vfixupimmss&ss=go
        - https://github.com/Barebit/x86reference/blob/master/x86reference.xml
        - https://github.com/llvm/llvm-project/blob/main/llvm/lib/Target/X86/X86.td
        - https://github.com/kframework/X86-64-semantics
    - perf info
        - https://www.agner.org/optimize/instruction_tables.ods
        - https://uops.info/
- ARM info
    - https://github.com/nspin/hs-arm
    - https://github.com/nspin/hs-arm/blob/8f10870a4afbbba010e78bd98e452ba67adc34e0/nix-results/harm.harm-tables-src/gen/Harm/Tables/Gen/Insn.hs
    - https://github.com/alastairreid/asl-interpreter
    - https://developer.arm.com/architectures/cpu-architecture/a-profile/exploration-tools
- Emulator/JIT frameworks:
    - assemblers:
        * bddisasm (C) [27] (https://github.com/bitdefender/bddisasm) - disassembly only
        * Capstone (C++) [22] (https://github.com/capstone-engine/capstone) - disassembly only
        * DynamoRIO (C and C++) [2] (https://github.com/DynamoRIO/dynamorio) - pyda bindings
        * fadec (C) [23] (https://github.com/aengelke/fadec) - written in assembly, no Python bindings
        * GNU libopcodes (C) [21] (https://www.gnu.org/software/binutils/) - no Python API
        * Iced (Rust) [28] (https://github.com/icedland/iced) - official Python bindings
        * Intel XED (C) [25] (https://github.com/intelxed/xed) - no Python bindings but has ctypes examples
        * Udis86 (C) [24] (https://github.com/vmt/udis86) - disassembly only, no Python bindings
        * Zydis (C) [26] (https://github.com/zyantific/zydis) - could write ctypes most likely, no examples
        * https://github.com/colejohnson66/rbx86
        * https://github.com/MahdiSafsafi/AMED
        * https://github.com/nidud/asmc
    - other stuff
        - https://bochs.sourceforge.io/
        - QEMU
        - https://github.com/asmjit/asmjit
        - https://github.com/dyninst/dyninst
        - https://github.com/herumi/xbyak
        - https://github.com/diegocarba99/bagheera
        - https://github.com/mongodb-labs/disasm

- abi and calling convention
    - https://stackoverflow.com/questions/2535989/what-are-the-calling-conventions-for-unix-linux-system-calls-and-user-space-f/2538212#2538212
    - https://manpages.debian.org/unstable/manpages-dev/syscall.2.en.html
    - https://gitlab.com/x86-psABIs/x86-64-ABI/-/jobs
    - https://gankra.github.io/blah/swift-abi/
    - libffi
    - https://github.com/rust-lang/rust-bindgen
    - https://github.com/rpav/c2ffi
    - https://github.com/aguinet/dragonffi
    - https://lwn.net/Articles/604287/
    - https://github.com/torvalds/linux/blob/141415d7379a02f0a75b1a7611d6b50928b3c46d/fs/io_uring.c#L9737
    

**0.1.1 Memory & Reference Representation #8**

- memory concurrency models
    - Peter Sewell, Susmit Sarkar, Scott Owens, Francesco Zappa Nardelli, and Magnus O. Myreen. X86-TSO: a rigorous and usable programmer's model for x86 multiprocessors. *Communications of the ACM*, 53(7):89--97, July 2010. URL: <https://dl.acm.org/doi/10.1145/1785414.1785443> (visited on 2021-07-09), [doi:10.1145/1785414.1785443](https://doi.org/10.1145/1785414.1785443).
    - Christopher Pulte, Shaked Flur, Will Deacon, Jon French, Susmit Sarkar, and Peter Sewell. Simplifying ARM concurrency: multicopy-atomic axiomatic and operational models for ARMv8. *Proceedings of the ACM on Programming Languages*, 2(POPL):19:1--19:29, December 2017. URL: <https://doi.org/10.1145/3158107> (visited on 2021-07-19), [doi:10.1145/3158107](https://doi.org/10.1145/3158107).
    - https://en.wikipedia.org/wiki/Hazard_(computer_architecture)#Data_hazards
    - https://www.cl.cam.ac.uk/~pes20/cpp/cpp0xmappings.html
    - https://gee.cs.oswego.edu/dl/jmm/cookbook.html
    - https://docs.oracle.com/javase/tutorial/essential/concurrency/locksync.html
    - https://stackoverflow.com/questions/46721075/can-modern-x86-hardware-not-store-a-single-byte-to-memory
    - https://gcc.gnu.org/pipermail/gcc-patches/2017-April/471979.html
- basic things
    - https://reverseengineering.stackexchange.com/questions/8297/proc-self-maps-equivalent-on-windows/8299
    - https://stackoverflow.com/questions/25852367/x86-64-canonical-address
    - https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-947.pdf CHERI pointers
    - https://en.wikipedia.org/wiki/Pointer_machine
    - Jeehoon Kang, Chung-Kil Hur, William Mansky, Dmitri Garbuzov, Steve Zdancewic, and Viktor Vafeiadis. A formal C memory model supporting integer-pointer casts. In *Proceedings of the 36th ACM SIGPLAN Conference on Programming Language Design and Implementation*, 326--335. Portland OR USA, June 2015. ACM. URL: <https://dl.acm.org/doi/10.1145/2737924.2738005> (visited on 2021-06-14), [doi:10.1145/2737924.2738005](https://doi.org/10.1145/2737924.2738005).
    - https://news.ycombinator.com/item?id=12388244 https://bactra.org//weblog/491.html
- memory allocation
    - https://www.forrestthewoods.com/blog/benchmarking-malloc-with-doom3/
    - Yu virtualalloc <https://github.com/alpha123/yu/tree/master/src/platform>
    - Go: <https://github.com/edsrzf/mmap-go>
    - C: mmap on windows <https://github.com/alitrack/mman-win32>
    - C++: <https://github.com/mandreyel/mio>
    - Rust: <https://github.com/RazrFalcon/memmap2-rs>
    - https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/unsafe-code#pointer-types
    - https://github.com/Mathnerd314/stroscot/blob/master/src/model/MemoryStatus.hs
    - https://valgrind.org/docs/manual/mc-manual.html#mc-manual.machine
    - https://docs.microsoft.com/en-us/windows/win32/procthread/using-thread-local-storage
    - https://developer.android.com/reference/android/util/SparseArray
    - https://github.com/Others/shredder
    - TelaMalloc paper
- binary representation
    - https://doc.rust-lang.org/reference/type-layout.html
    - https://wingolog.org/archives/2011/05/18/value-representation-in-javascript-implementations
    - https://searchfox.org/mozilla-central/source/js/public/Value.h#526
    - https://hackage.haskell.org/package/binary-0.8.9.1/docs/src/Data.Binary.Get.Internal.html#Decoder
    - https://github.com/mgsloan/store/blob/master/store-core/src/Data/Store/Core.hs
    - Benjamin Delaware, Sorawit Suriyakarn, Clément Pit-Claudel, Qianchuan Ye, and Adam Chlipala. Narcissus: correct-by-construction derivation of decoders and encoders from binary formats. *Proceedings of the ACM on Programming Languages*, 3(ICFP):1--29, July 2019. URL: <https://www.cs.purdue.edu/homes/bendy/Narcissus/narcissus.pdf> (visited on 2020-07-26), [doi:10.1145/3341686](https://doi.org/10.1145/3341686).
    - https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p2410r0.pdf
    - https://github.com/ollef/sixten
    - https://en.wikipedia.org/wiki/Type_punning
    - https://github.com/Ed-von-Schleck/shoco
    - http://www.sequitur.info/
- finalizers and static management
    - Raphaël L Proust. ASAP: As Static As Possible memory management. Technical Report UCAM-CL-TR-908, University of Cambridge Computer Laboratory, July 2017. URL: <https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-908.pdf>.
    - https://en.wikipedia.org/wiki/Finalizer
    - Nathan Corbyn. *Practical Static Memory Management*. Bachelor's Thesis, King's College, May 2020. URL: <http://nathancorbyn.com/nc513.pdf>.
        - Micro-mitten: ASAP implementation for a simple Rust like language. Some promising results but his implementation was quite flawed so overall results were negative. <https://github.com/doctorn/micro-mitten>
    - Samuel Z. Guyer, Kathryn S. McKinley, and Daniel Frampton. Free-Me: a static analysis for automatic individual object reclamation. In *Proceedings of the 27th ACM SIGPLAN Conference on Programming Language Design and Implementation*, PLDI '06, 364--375. New York, NY, USA, June 2006. Association for Computing Machinery. URL: <https://doi.org/10.1145/1133981.1134024> (visited on 2023-07-18), [doi:10.1145/1133981.1134024](https://doi.org/10.1145/1133981.1134024).
    - Aaron Weiss, Daniel Patterson, Nicholas D. Matsakis, and Amal Ahmed. Oxide: the essence of Rust. *arXiv:1903.00982 [cs]*, March 2019. Comment: In submission. URL: <http://arxiv.org/abs/1903.00982> (visited on 2020-05-17), [arXiv:1903.00982](https://arxiv.org/abs/1903.00982).
    - https://rcoh.me/posts/rust-linked-list-basically-impossible/
    - https://rust-unofficial.github.io/too-many-lists/third-layout.html
    - https://arxiv.org/pdf/1901.01001.pdf
    - https://arxiv.org/pdf/2011.06171.pdf
    - https://dl.acm.org/doi/pdf/10.1145/3510003.3510107
- gc and refcounting vs. manual
    - Andrew W. Appel. Garbage collection can be faster than stack allocation. *Information Processing Letters*, 25(4):275--279, June 1987. URL: <https://www.cs.princeton.edu/~appel/papers/45.pdf> (visited on 2020-07-24), [doi:10.1016/0020-0190(87)90175-X](https://doi.org/10.1016/0020-0190(87)90175-X).
    - Perceus algorithm is used in the Koka language <https://github.com/koka-lang/koka>
    - Counting Immutable Beans: Reference Counting Optimized for Purely Functional Programming <https://arxiv.org/abs/1908.05647>
    - Perceus: Garbage Free Reference Counting with Reuse <https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf>
    - Cliff Click, Gil Tene, and Michael Wolf. The pauseless GC algorithm. In *Proceedings of the 1st ACM/USENIX International Conference on Virtual Execution Environments - VEE '05*, 46. Chicago, IL, USA, 2005. ACM Press. URL: <https://static.usenix.org/events/vee05/full_papers/p46-click.pdf> (visited on 2021-04-25), [doi:10.1145/1064979.1064988](https://doi.org/10.1145/1064979.1064988).
    - Gil Tene, Balaji Iyengar, and Michael Wolf. C4: the continuously concurrent compacting collector. *ACM SIGPLAN Notices*, 46(11):79--88, June 2011. URL: <https://doi.org/10.1145/2076022.1993491> (visited on 2022-06-08), [doi:10.1145/2076022.1993491](https://doi.org/10.1145/2076022.1993491).
    - https://www.gnu.org/software/libc/manual/html_node/Obstacks.html
- architecture notes
    - Zhaomo Yang, Brian Johannesmeyer, Anders Trier Olesen, Sorin Lerner, and Kirill Levchenko. Dead store elimination (still) considered harmful. In *26th USENIX Security Symposium*. Vancouver, BC, Canada, August 2017.
    - Tomasz Jurkiewicz and Kurt Mehlhorn. The cost of address translation. *arXiv:1212.0703 [cs]*, April 2014. Comment: A extended abstract of this paper was published in the proceedings of ALENEX13, New Orleans, USA. URL: <http://arxiv.org/abs/1212.0703> (visited on 2022-01-04), [arXiv:1212.0703](https://arxiv.org/abs/1212.0703).
- aggregate update optimization
    - Amir M. Ben-amram. Notes on Pippenger's comparison of pure and impure LISP. 1996. URL: [citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.55.3024&rep=rep1&type=pdf](https://mathnerd314.github.io/stroscot/citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.55.3024&rep=rep1&type=pdf).
    - Nicholas Pippenger. Pure versus impure Lisp. *ACM Transactions on Programming Languages and Systems*, 19(2):223--238, March 1997. URL: <https://dl.acm.org/doi/10.1145/244795.244798> (visited on 2022-01-06), [doi:10.1145/244795.244798](https://doi.org/10.1145/244795.244798).
    - Paul Hudak and Adrienne Bloss. The aggregate update problem in functional programming systems. In *Proceedings of the 12th ACM SIGACT-SIGPLAN Symposium on Principles of Programming Languages - POPL '85*, 300--314. New Orleans, Louisiana, United States, 1985. ACM Press. URL: <http://portal.acm.org/citation.cfm?doid=318593.318660> (visited on 2022-01-04), [doi:10.1145/318593.318660](https://doi.org/10.1145/318593.318660).
    - Milan Straka. *Functional Data Structures and Algorithms*. PhD thesis, Computer Science Institute of Charles University, September 2013. URL: <https://dspace.cuni.cz/handle/20.500.11956/52896> (visited on 2022-01-04).
    - https://prog21.dadgum.com/41.html


- https://ziglang.org/documentation/master/#Memory
- https://drewdevault.com/2020/01/04/Slow.html
- https://utcc.utoronto.ca/~cks/space/blog/programming/Go116OpenBSDUsesLibc
- https://marcan.st/2017/12/debugging-an-evil-go-runtime-bug
- https://filippo.io/linux-syscall-table/
- https://pkg.go.dev/golang.org/x/sys/unix?utm_source=godoc
- https://cs.opensource.google/go/x/sys/+/master:unix/linux/mkall.go
- https://utcc.utoronto.ca/~cks/space/blog/programming/GoCGoCompatibleStructs

- aliasing
- pointer machines, functional data structures

These are directly relevant to how the P‑code machine represents references, heap objects, and interacts with host memory.

**0.1.2 Process Virtualization #9**

- fibers, threads, concurrency
    - Gor Nishanov. Fibers under the magnifying glass. November 2018. URL: <http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2018/p1364r0.pdf> (visited on 2021-11-04). section 2.3.1
    - http://jdebp.info/FGA/linux-control-groups-are-not-jobs.html
    - https://devblogs.microsoft.com/oldnewthing/20191011-00/?p=102989
    - https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch02.html
    - https://en.wikipedia.org/wiki/Communicating_sequential_processes
    - https://github.com/apple/swift-evolution/blob/main/proposals/0304-structured-concurrency.md#motivation
    - https://docs.python.org/3/library/threading.html#threading.BoundedSemaphore
- worlds and stores
    - Worlds: Controlling the Scope of Side Effects, {Warth, Alessandro and Ohshima, Yoshiki and Kaehler, Ted and Kay, Alan}, {2011}, 10.1007/978-3-642-22655-7_9
    - J. Gregory Morrisett. Refining first-class stores. In *Proceedings of the ACM SIGPLAN Workshop on State in Programming Languages*, 73--87. Citeseer, 1993.
    - G. F. Johnson and D. Duggan. Stores and partial continuations as first-class objects in a language and its environment. In *Proceedings of the 15th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages*, POPL '88, 158--168. New York, NY, USA, January 1988. Association for Computing Machinery. URL: <https://doi.org/10.1145/73560.73574> (visited on 2022-12-02), [doi:10.1145/73560.73574](https://doi.org/10.1145/73560.73574).
    - Gregory F Johnson and Dominic Duggan. First-class stores and partial continuations in a programming language and environment. *Computer Languages*, 20(1):53--68, March 1994. URL: <https://linkinghub.elsevier.com/retrieve/pii/0096055194900140> (visited on 2022-12-05), [doi:10.1016/0096-0551(94)90014-0](https://doi.org/10.1016/0096-0551(94)90014-0).
    - https://criu.org/Images
    - http://r6.ca/blog/20071028T162529Z.html
    - https://www.mail-archive.com/haskell-cafe@haskell.org/msg66512.html
- jvm stuff and how to abstract away the OS
    - https://www.javamadesoeasy.com/2015/06/jvm-java-virtual-machine.html
    - https://docs.oracle.com/en/java/javase/12/vm/java-virtual-machine-technology-overview.html
    - https://www.geeksforgeeks.org/java/how-jvm-works-jvm-architecture/
    - https://www.reddit.com/r/haskell/comments/1irfu72/ghc_how_to_build_a_native_compiler_for_a_new/
    - https://www.oracle.com/java/technologies/whitepaper.html
    - https://docs.google.com/document/d/1wAaf1rYoM4S4gtnPh0zOlGzWtrZFQ5suE8qr2sD8uWQ/pub Contiguous stacks in Go

- (free) monads and algebraic effects
    - Roman Elizarov, Mikhail Belyaev, Marat Akhin, and Ilmir Usmanov. Kotlin coroutines: design and implementation. In *Proceedings of the 2021 ACM SIGPLAN International Symposium on New Ideas, New Paradigms, and Reflections on Programming and Software*, Onward! 2021, 68--84. New York, NY, USA, October 2021. Association for Computing Machinery. URL: <https://doi.org/10.1145/3486607.3486751> (visited on 2022-11-29), [doi:10.1145/3486607.3486751](https://doi.org/10.1145/3486607.3486751).
    - R. Kent Dyvbig, Simon Peyton Jones, and Amr Sabry. A monadic framework for delimited continuations. *Journal of Functional Programming*, 17(6):687--730, November 2007. URL: <https://doi.org/10.1017/S0956796807006259> (visited on 2020-06-19), [doi:10.1017/S0956796807006259](https://doi.org/10.1017/S0956796807006259).
    - https://stackoverflow.com/questions/25827227/why-cant-there-be-an-instance-of-monadfix-for-the-continuation-monad
    - Paul Hudak, John Hughes, Simon Peyton Jones, and Philip Wadler. A history of Haskell: being lazy with class. In *Proceedings of the Third ACM SIGPLAN Conference on History of Programming Languages*. San Diego California, June 2007. ACM. URL: <https://dl.acm.org/doi/10.1145/1238844.1238856> (visited on 2022-06-03), [doi:10.1145/1238844.1238856](https://doi.org/10.1145/1238844.1238856).
    - https://www.eyrie.org/%7Ezednenem/2013/06/prompt
    - https://www.reddit.com/r/haskell/comments/5a5frc/a_correct_free_monad_and_free_monad_fix/
    - https://web.archive.org/web/20220124082435/http://comonad.com/reader/2011/free-monads-for-less/
    - http://comonad.com/reader/2011/free-monads-for-less-2/
    - Exequiel Rivas and Mauro Jaskelioff. Notions of computation as monoids. *Journal of Functional Programming*, May 2014. URL: <https://www.cambridge.org/core/journals/journal-of-functional-programming/article/notions-of-computation-as-monoids/70019FC0F2384270E9F41B9719042528> (visited on 2022-06-08), [doi:10.1017/S0956796817000132](https://doi.org/10.1017/S0956796817000132).
    - Yannick Forster, Ohad Kammar, Sam Lindley, and Matija Pretnar. On the expressive power of user-defined effects: Effect handlers, monadic reflection, delimited control. *arXiv:1610.09161 [cs]*, February 2017. URL: <http://arxiv.org/abs/1610.09161> (visited on 2021-11-29), [arXiv:1610.09161](https://arxiv.org/abs/1610.09161).
- promises and function colors
    - https://fa.haskell.narkive.com/hUgYjfKJ/haskell-cafe-the-mother-of-all-functors-monads-categories#post3
    - https://buzzdecafe.github.io/2018/04/10/no-promises-are-not-monads
    - https://github.com/promises-aplus/promises-spec/issues/94
    - https://stackoverflow.com/a/49703783
    - http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/
    - https://gavinhoward.com/2022/04/i-believe-zig-has-function-colors/
    - https://github.com/dmitriz/cpsfy/blob/master/DOCUMENTATION.md
    - https://www.youtube.com/watch?v=QNpKYypLAO8
    - Tarmo Uustalu and Varmo Vene. Comonadic notions of computation. *Electronic Notes in Theoretical Computer Science*, 203(5):263--284, June 2008. URL: <https://linkinghub.elsevier.com/retrieve/pii/S1571066108003435> (visited on 2023-01-17), [doi:10.1016/j.entcs.2008.05.029](https://doi.org/10.1016/j.entcs.2008.05.029).
    - https://news.ycombinator.com/item?id=8985436
    - https://gavinhoward.com/2022/04/i-believe-zig-has-function-colors/#review-of-function-colors


operating system concurrency control.
- JVM architecture overview, GHC RTS commentary, Zig runtime and build internals; materials on virtual machines and execution environments.

These inform how a “process” in your target runtime is virtualized and scheduled over OS threads/processes.

**0.1.3 Exceptions, Traps, and Error Handling #10**

- https://github.com/torvalds/linux/blob/a931dd33d370896a683236bba67c0d6f3d01144d/arch/x86/kernel/traps.c
- https://docs.microsoft.com/en-us/cpp/cpp/structured-exception-handling-c-cpp?view=msvc-160
- https://reviews.llvm.org/D102817
- https://docs.microsoft.com/en-us/windows/win32/debug/vectored-exception-handling
- https://github.com/ziglang/zig/blob/e2b954c2738c683a85b864eb33530f0e3dbbc480/lib/std/debug.zig#L1527
- https://llvm.org/docs/LangRef.html#poisonvalues
- https://www.youtube.com/watch?v=yG1OZ69H_-o
- https://blog.llvm.org/2011/05/what-every-c-programmer-should-know_21.html
- https://docs.microsoft.com/en-us/dotnet/api/system.threading.thread.abort?view=net-6.0
- https://man7.org/linux/man-pages/man7/signal.7.html
- https://web.archive.org/web/20180621011720/http://sunfishcode.github.io/blog/2014/07/14/undef-introduction.html
- https://compcert.org/man/manual004.html
- https://github.com/rust-lang/rfcs/blob/master/text/0560-integer-overflow.md
- http://joeduffyblog.com/2009/06/23/concurrency-and-exceptions/
- https://www.python.org/dev/peps/pep-0654
- https://lkml.org/lkml/2022/9/19/1105
- https://lkml.org/lkml/2022/9/19/1250
- https://doc.rust-lang.org/reference/expressions/operator-expr.html#the-question-mark-operator
- https://stackoverflow.com/a/6793463
- Simon Peyton Jones, Alastair Reid, Fergus Henderson, Tony Hoare, and Simon Marlow. A semantics for imprecise exceptions. *ACM SIGPLAN Notices*, 34(5):25--36, May 1999. URL: <https://doi.org/10.1145/301631.301637> (visited on 2022-07-25), [doi:10.1145/301631.301637](https://doi.org/10.1145/301631.301637).
- https://github.com/apple/swift/blob/main/docs/ErrorHandlingRationale.md#kinds-of-propagation
- https://shipilev.net/blog/2014/exceptional-performance/
- https://stackoverflow.com/questions/37378035/how-to-signal-a-specific-thread-in-windows
- https://stackoverflow.com/questions/3444190/windows-suspendthread-doesnt-getthreadcontext-fails
- https://repnz.github.io/posts/apc/user-apc/
- https://mail.haskell.org/pipermail/libraries/2010-March/013310.html
- https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/
- https://hackage.haskell.org/package/unliftio-0.2.13.1/docs/UnliftIO-Exception.html#v:bracket
- https://llvm.org/devmtg/2020-09/slides/Lee-UndefPoison.pdf
- https://grenouillebouillie.wordpress.com/2022/05/09/the-hidden-cost-of-exception-handling/
- https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html
- https://en.wikipedia.org/wiki/Chaos_engineering
- https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/
- Item 16 "Destructors That Throw and Why They're Evil" of 1999 book Exceptional C++
- https://ptgmedia.pearsoncmg.com/imprint_downloads/informit/aw/meyerscddemo/DEMO/MAGAZINE/SUTTER.HTM
- http://web.archive.org/web/20130728131646/cpp-next.com/archive/2012/08/evil-or-just-misunderstood/
- https://en.wikipedia.org/wiki/Exception_safety
- http://www.open-std.org/jtc1/sc22/wg21/docs/papers/1995/N0623.asc
- https://github.com/rust-lang/rfcs/blob/master/text/1236-stabilize-catch-panic.md#background-what-is-exception-safety-in-rust
- https://www.joelonsoftware.com/2003/10/13/13/
- https://groups.google.com/g/golang-nuts/c/HOXNBQu5c-Q/m/ltQ-QHBrw9gJ
- https://www.stroustrup.com/bs_faq2.html#finally
- http://web.archive.org/web/20060101083304/http://discuss.develop.com/archives/wa.exe?A2=ind0011A&L=DOTNET&P=R32820


***

### 0.2 Graph‑Based Optimal Reduction Engine and IR

**0.2 Graph‑Based Optimal Reduction Engine and IR #11**

- The Bologna Optimal Higher-Order Machine, Asperti, Andrea and Giovannetti, Cecilia and Naletto, Andrea
- Ian Mackie. The geometry of interaction machine. In *Proceedings of the 22nd ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages - POPL '95*, 198--208. San Francisco, California, United States, 1995. ACM Press. URL: <http://portal.acm.org/citation.cfm?doid=199448.199483> (visited on 2023-04-28), [doi:10.1145/199448.199483](https://doi.org/10.1145/199448.199483).
- Stefano Guerrini. *Theoretical and Practical Issues of Optimal Implementations of Functional Languages*. PhD thesis, Università di Pisa. Dipartimento di Informatica, 1996. URL: <https://www-lipn.univ-paris13.fr/~guerrini/mysite/sites/default/files/biblio/PhDThesis.pdf>.
- Andrea Asperti and Stefano Guerrini. *The Optimal Implementation of Functional Programming Languages*. Number 45 in Cambridge Tracts in Theoretical Computer Science. Cambridge University Press, USA, 1st edition, January 1999. ISBN 978-0-521-62112-0.
- Jean-Yves Girard. Towards a geometry of interaction. In J. W. Gray and A. Scedrov, editors, *Categories in Computer Science and Logic*, volume 92 of Contemporary Mathematics, 69--108. University of Colorado in Boulder, 1989. American Mathematical Society. Proceedings of the AMS-IMS-SIAM Joint Summer Research Conference, June 14--20, 1987, Boulder, Colorado; Contemporary Mathematics Volume 92. URL: <https://jb55.com/linear/pdf/Towards%20a%20geometry%20of%20interaction.pdf>.
- Andrea Asperti and Harry G. Mairson. Parallel beta reduction is not elementary recursive. *Information and Computation*, 170(1):49--80, October 2001. URL: <https://linkinghub.elsevier.com/retrieve/pii/S089054010192869X> (visited on 2020-06-15), [doi:10.1006/inco.2001.2869](https://doi.org/10.1006/inco.2001.2869).
- Olin Shivers and Mitchell Wand. Bottom-up β-reduction: uplinks and λ-DAGs (extended version). Technical Report, Citeseer, 2004.
- Is the Optimal Implementation Inefficient? {{Elementarily}} Not. Guerrini, Stefano and Solieri, Marco, 2017 http://drops.dagstuhl.de/opus/volltexte/2017/7733/
- Paolo Coppola. *On the Complexity of Optimal Reduction of Functional Programming Languages*. PhD thesis, Università degli Studi di Udine, February 2002.
- Optimal {{Sharing Graphs}} for {{Substructural Higher-order Rewriting Systems}}}, Smith, Connor Lane, {2017}, https://kar.kent.ac.uk/63884/
- A Unified Approach to Fully Lazy Sharing, Balabonski, Thibaut, 2011,
- Simon Peyton Jones. *The Implementation of Functional Programming Languages*. Prentice-Hall, 1987. URL: <https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/> (visited on 2020-08-02).
- Paul Blain Levy. Jumbo λ-calculus. In Michele Bugliesi, Bart Preneel, Vladimiro Sassone, and Ingo Wegener, editors, *Automata, Languages and Programming*, Lecture Notes in Computer Science, 444--455. Berlin, Heidelberg, 2006. Springer. URL: <https://www.cs.bham.ac.uk/~pbl/papers/jumboicalp.pdf>, [doi:10.1007/11787006_38](https://doi.org/10.1007/11787006_38).
- Oleg Kiselyov. Many faces of the fixed-point combinator. August 2013. URL: <http://okmij.org/ftp/Computation/fixed-point-combinators.html> (visited on 2020-07-31).
Sequent Calculus as a Compiler Intermediate Language, Downen, Paul and Maurer, Luke and Ariola, Zena M. and Peyton Jones, Simon, 2016
https://ncatlab.org/nlab/show/mix+rule
https://www.pls-lab.org/en/Mix_rule
Lutz STRAßBURGER. Deep inference and expansion trees for second-order multiplicative linear logic. *Mathematical Structures in Computer Science*, 29(8):1030--1060, September 2019. URL: <https://www.cambridge.org/core/product/identifier/S0960129518000385/type/journal_article> (visited on 2022-08-11), [doi:10.1017/S0960129518000385](https://doi.org/10.1017/S0960129518000385).
Eben Blaisdell, Max Kanovich, Stepan L. Kuznetsov, Elaine Pimentel, and Andre Scedrov. Non-associative, non-commutative multi-modal linear logic. In Jasmin Blanchette, Laura Kovács, and Dirk Pattinson, editors, *Automated Reasoning*, Lecture Notes in Computer Science, 449--467. Cham, 2022. Springer International Publishing. [doi:10.1007/978-3-031-10769-6_27](https://doi.org/10.1007/978-3-031-10769-6_27).
https://en.wikipedia.org/wiki/Difference_list
Philip Wadler. Call-by-value is dual to call-by-name. In *Proceedings of the Eighth ACM SIGPLAN International Conference on Functional Programming*, ICFP '03, 189--201. Uppsala, Sweden, August 2003. Association for Computing Machinery. URL: <http://homepages.inf.ed.ac.uk/wadler/papers/dual/dual.pdf> (visited on 2020-06-17), [doi:10.1145/944705.944723](https://doi.org/10.1145/944705.944723).
Tristan Crolard. A formulae-as-types interpretation of subtractive logic. *Journal of Logic and Computation*, 14(4):529--570, August 2004. URL: <https://academic.oup.com/logcom/article/14/4/529/933555> (visited on 2020-06-18), [doi:10.1093/logcom/14.4.529](https://doi.org/10.1093/logcom/14.4.529).
Roberto Di Cosmo. *Introduction to Linear Logic*. MPRI course notes, 2015. URL: <https://www.dicosmo.org/CourseNotes/LinLog/IntroductionLinearLogic.pdf> (visited on 2020-05-13).
https://en.wikipedia.org/wiki/Monoidal_category
https://www.seas.upenn.edu/~sweirich/types/archive/1991/msg00123.html
John Maraist, Martin Odersky, David N. Turner, and Philip Wadler. Call-by-name, call-by-value, call-by-need, and the linear lambda calculus. *Electronic Notes in Theoretical Computer Science*, 1:370--392, January 1995. URL: <http://www.sciencedirect.com/science/article/pii/S1571066104000222> (visited on 2020-08-23), [doi:10.1016/S1571-0661(04)00022-2](https://doi.org/10.1016/S1571-0661(04)00022-2).
Andrzej Filinski. Linear continuations. In *Proceedings of the 19th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages*, POPL '92, 27--38. Albuquerque, New Mexico, USA, February 1992. Association for Computing Machinery. URL: <https://doi.org/10.1145/143165.143174> (visited on 2020-06-19), [doi:10.1145/143165.143174](https://doi.org/10.1145/143165.143174).
Vincent Danos, Jean-Baptiste Joinet, and Harold Schellinx. The structure of exponentials: uncovering the dynamics of linear logic proofs. In G. Gottlob, A. Leitsch, and D. Mundici, editors, *Proceedings of the Third Kurt Gödel Colloquium on Computational Logic and Proof Theory*, 159--171. Brno, Czech Republic, August 1993. Springer-Verlag LNCS 348. URL: <https://eprints.illc.uva.nl/1334/>.
Marcelo Coniglio and Francisco Miraglia. Equality in linear logic. *Logique et Analyse*, 39(153/154):113--151, January 2002. URL: <https://www.researchgate.net/profile/Marcelo-Coniglio/publication/2387274_Equality_In_Linear_Logic/links/0deec5165a6babbd8a000000/Equality-In-Linear-Logic.pdf>.
Yves Lafont. Linear logic pages. URL: <http://iml.univ-mrs.fr/~lafont/pub/llpages.pdf>.
Z A Sparks and Amr Sabry. Superstructural reversible logic. In *3rd International Workshop on Linearity*. 2014. URL: <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.720.5692&rep=rep1&type=pdf>.
Roshan P James and Amr Sabry. Theseus: a high level language for reversible computing. In *Work-in-Progress Report at Conference on Reversible Computation*, 12. 2014. URL: <https://legacy.cs.indiana.edu/~sabry/papers/theseus.pdf>.
Chao-Hong Chen and Amr Sabry. A computational interpretation of compact closed categories: reversible programming with negative and fractional types. *Proceedings of the ACM on Programming Languages*, 5(POPL):1--29, January 2021. URL: <https://dl.acm.org/doi/10.1145/3434290> (visited on 2021-08-04), [doi:10.1145/3434290](https://doi.org/10.1145/3434290).
https://github.com/DreamLinuxer/popl21-artifact
S. Martini and A. Masini. On the fine structure of the exponential rule. In Jean-Yves Girard, Yves Lafont, and Laurent Regnier, editors, *Advances in Linear Logic*, pages 197--210. Cambridge University Press, Cambridge, 1995. URL: <https://pdfs.semanticscholar.org/b2cb/538c8ef21af42e48134a17a3c62ce5167837.pdf> (visited on 2020-08-01), [doi:10.1017/CBO9780511629150.010](https://doi.org/10.1017/CBO9780511629150.010).
https://en.wikipedia.org/wiki/Tensor_algebra#Construction
https://ncatlab.org/nlab/show/sequent+calculus
Masaru Shirahata. *Linear Set Theory*. PhD thesis, Stanford University, February 1994. URL: <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.29.7077&rep=rep1&type=pdf>.
Michael Shulman. Linear logic for constructive mathematics. *arXiv:1805.07518 [math]*, May 2018. Comment: 39 pages. URL: <http://arxiv.org/abs/1805.07518> (visited on 2021-03-04), [arXiv:1805.07518](https://arxiv.org/abs/1805.07518).
Thomas Forster. Quine's New Foundations. In Edward N. Zalta, editor, *The Stanford Encyclopedia of Philosophy*. Metaphysics Research Lab, Stanford University, summer 2019 edition, 2019. URL: <https://plato.stanford.edu/archives/sum2019/entries/quine-nf/> (visited on 2021-03-04).
M. Randall Holmes. *Elementary Set Theory with a Universal Set*. Bruylant-Academia, 1998. ISBN 978-2-87209-488-2. URL: <https://randall-holmes.github.io/head.pdf>.
https://en.wikipedia.org/wiki/Use-define_chain
Masaru Shirahata. A linear conservative extension of Zermelo-Fraenkel set theory. *Studia Logica*, 56(3):361--392, May 1996. URL: <http://link.springer.com/10.1007/BF00372772> (visited on 2021-03-06), [doi:10.1007/BF00372772](https://doi.org/10.1007/BF00372772).
https://github.com/Mathnerd314/stroscot/blob/fb648be1ecc3e5c062dbb000d6887a2ce7ac7eb0/src/Core.hs#L50

**0.2.1 Term Rewriting & Strategy Module #12**

- Term rewriting and all that Book by Franz Baader
- V. van Oostrom. *Confluence for Abstract and Higher-Order Rewriting*. PhD thesis, Vrije Universiteit Amsterdam, 1994.
- Coinductive Foundations of Infinitary Rewriting and Infinitary Equational Logic, {Endrullis, Jörg and Hansen, Helle Hvid and Hendriks, Dimitri and Polonsky, Andrew and Silva, Alexandra}, {2018},
- Marissa Venturini Zilli. Reduction graphs in the lambda calculus. *Theoretical Computer Science*, 29(3):251--275, January 1984. URL: <https://www.sciencedirect.com/science/article/pii/0304397584900021> (visited on 2022-06-08), [doi:10.1016/0304-3975(84)90002-1](https://doi.org/10.1016/0304-3975(84)90002-1).
- Nachum Dershowitz, Stéphane Kaplan, and David A. Plaisted. Rewrite, rewrite, rewrite, rewrite, rewrite, .... *Theoretical Computer Science*, 83(1):71--96, June 1991. URL: <https://www.sciencedirect.com/science/article/pii/0304397591900409> (visited on 2022-07-02), [doi:10.1016/0304-3975(91)90040-9](https://doi.org/10.1016/0304-3975(91)90040-9).
- https://en.wikipedia.org/wiki/Strongly_connected_component#Definitions
- Jeroen Ketema, Jan Willem Klop, and Vincent van Oostrom. Vicious circles in rewriting systems. In *5th International Workshop on Reduction Strategies in Rewriting and Programming*, 20. Nara, Japan, April 2005. URL: <https://www.dicosmo.org/WRS05/proceedings/jeroen.pdf>.
- https://en.wikipedia.org/wiki/Ultrametric_space
- https://oscarcunningham.com/494/a-better-representation-for-real-numbers/
- Stefan Kahrs. Infinitary rewriting: closure operators, equivalences and models. *Acta Informatica*, 50(2):123--156, March 2013. URL: <http://link.springer.com/10.1007/s00236-012-0174-y> (visited on 2022-10-22), [doi:10.1007/s00236-012-0174-y](https://doi.org/10.1007/s00236-012-0174-y).
- https://en.wikipedia.org/wiki/Adherent_point
- {Rewrite Systems},   booktitle = {Handbook of Theoretical Computer Science (Vol. {{B}}): Formal Models and Semantics}, {Dershowitz, Nachum and Jouannaud, Jean-Pierre},= {1991}, https://www.cs.tau.ac.il/~nachum/papers/survey-draft.pdf
- Jakob Grue Simonsen. Weak convergence and uniform normalization in infinitary rewriting. In Christopher Lynch, editor, *Proceedings of the 21st International Conference on Rewriting Techniques and Applications*, volume 6 of Leibniz International Proceedings in Informatics (LIPIcs), 311--324. Dagstuhl, Germany, 2010. Schloss Dagstuhl--Leibniz-Zentrum fuer Informatik. URL: <http://drops.dagstuhl.de/opus/volltexte/2010/2660> (visited on 2022-07-25), [doi:10.4230/LIPIcs.RTA.2010.311](https://doi.org/10.4230/LIPIcs.RTA.2010.311).
- A Coinductive Treatment of Infinitary Term Rewriting  Endrullis, J. and Hansen, H. H. and Hendriks, D. and Polonsky, A. and Silva, A. 2013 https://repository.ubn.ru.nl/handle/2066/122925
- Infinitary Term Rewriting for Weakly Orthogonal Systems: Properties and Counterexamples Endrullis, Jörg and Grabmayer, Clemens and Hendriks, Dimitri and Klop, Jan Willem and Oostrom, Vincent 2014 https://lmcs.episciences.org/752
- Jan Willem Klop. *Combinatory Reduction Systems*. PhD thesis, Rijksuniversiteit Utrecht, June 1980. URL: <https://eprints.illc.uva.nl/id/eprint/1876/> (visited on 2022-09-24).
- https://en.wikipedia.org/wiki/Kuratowski_convergence
- https://mathoverflow.net/questions/179123/continuous-relations
- https://en.wikipedia.org/wiki/Hausdorff_distance
- R. Kennaway, V. van Oostrom, and F.-J. de Vries. Meaningless terms in rewriting. *Artificial Intelligence Preprint Series*, May 1999. URL: <https://dspace.library.uu.nl/handle/1874/26465> (visited on 2022-07-09).
- Paula Severi and Fer-Jan de Vries. Decomposing the lattice of meaningless sets in the infinitary lambda calculus. In Lev D. Beklemishev and Ruy de Queiroz, editors, *Logic, Language, Information and Computation*, Lecture Notes in Computer Science, 210--227. Berlin, Heidelberg, 2011. Springer. [doi:10.1007/978-3-642-20920-8_22](https://doi.org/10.1007/978-3-642-20920-8_22).
- A. Bucciarelli, A. Carraro, G. Favro, and A. Salibra. Graph easy sets of mute lambda terms. *Theoretical Computer Science*, 629:51--63, May 2016. URL: <https://www.sciencedirect.com/science/article/pii/S0304397515011858> (visited on 2022-07-01), [doi:10.1016/j.tcs.2015.12.024](https://doi.org/10.1016/j.tcs.2015.12.024).
- J.R. Kennaway, J.W. Klop, M.R. Sleep, and F.J. de Vries. Infinitary lambda calculus. *Theoretical Computer Science*, 175(1):93--125, March 1997. URL: <https://linkinghub.elsevier.com/retrieve/pii/S0304397596001715> (visited on 2022-11-05), [doi:10.1016/S0304-3975(96)00171-5](https://doi.org/10.1016/S0304-3975(96)00171-5).
- Jan Willem Klop and Roel C. de Vrijer. Infinitary normalization. In Sergei N. Artëmov, Howard Barringer, Artur S. d'Avila Garcez, Luís C. Lamb, and John Woods, editors, *We Will Show Them! Essays in Honour of Dov Gabbay, Volume Two*, 169--192. College Publications, 2005.
- Aart Middeldorp. Modular aspects of properties of term rewriting systems related to normal forms. In G. Goos, J. Hartmanis, D. Barstow, W. Brauer, P. Brinch Hansen, D. Gries, D. Luckham, C. Moler, A. Pnueli, G. Seegmüller, J. Stoer, N. Wirth, and Nachum Dershowitz, editors, *Rewriting Techniques and Applications*, volume 355, pages 263--277. Springer Berlin Heidelberg, Berlin, Heidelberg, 1989. URL: <http://link.springer.com/10.1007/3-540-51081-8_113> (visited on 2021-09-14), [doi:10.1007/3-540-51081-8_113](https://doi.org/10.1007/3-540-51081-8_113).
- Colin Stirling. Decidability of higher-order matching. *Logical Methods in Computer Science*, 5(3):2, July 2009. URL: <https://lmcs.episciences.org/757> (visited on 2021-11-15), [doi:10.2168/LMCS-5(3:2)2009](https://doi.org/10.2168/LMCS-5(3:2)2009).
- Petar Vukmirović, Alexander Bentkamp, and Visa Nummelin. Efficient full higher-order unification. *Logical Methods in Computer Science*, December 2021. URL: <https://lmcs.episciences.org/8837/pdf> (visited on 2022-06-14), [doi:10.46298/lmcs-17(4:18)2021](https://doi.org/10.46298/lmcs-17(4:18)2021).
- https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm
- https://en.wikipedia.org/wiki/Occurs_check
- W.P. Weijland. Semantics for logic programs without occur check. *Theoretical Computer Science*, 71(1):155--174, March 1990. URL: <https://linkinghub.elsevier.com/retrieve/pii/030439759090194M> (visited on 2022-07-15), [doi:10.1016/0304-3975(90)90194-M](https://doi.org/10.1016/0304-3975(90)90194-M).
- Krzysztof R. Apt and Alessandro Pellegrini. Why the occur-check is not a problem. In Maurice Bruynooghe and Martin Wirsing, editors, *Programming Language Implementation and Logic Programming*, volume 631, pages 69--86. Springer-Verlag, Berlin/Heidelberg, 1992. URL: <http://link.springer.com/10.1007/3-540-55844-6_128> (visited on 2022-07-14), [doi:10.1007/3-540-55844-6_128](https://doi.org/10.1007/3-540-55844-6_128).
- https://termination-portal.org/wiki/Termination_Competition_2023
- https://en.wikipedia.org/wiki/Strongly_connected_component#Algorithms
- V. van Oostrom. Developing developments. TCS, 175(1):159–181, 1997.
- V. van Oostrom and F. van Raamsdonk. Weak orthogonality implies confluence: The higher order case. In Proc. 3rd LFCS, volume 813 of LNCS, pages 379–392, 1994.
- Cynthia Kop. Higher Order Termination. PhD thesis, Vrije Universiteit, Amsterdam, 2012
- Nagele, B. Felgenhauer, and A. Middeldorp. Improving automatic confluence analysis of rewrite systems by redundant rules. In Proc. 26th RTA, volume 36 of LIPIcs, pages 257–268, 2015.
- Nao Hirokawa and Aart Middeldorp. Strategies for decreasingly confluent rewrite systems. *Reduction Strategies in Rewriting and Programming*, pages 23, 2011. URL: <http://elp.webs.upv.es/workshops/wrs2011/pre-proceedings.pdf#page=31>.
- https://cs.stackexchange.com/questions/99492/confluence-of-beta-expansion
- https://en.wikipedia.org/wiki/Normal_form_(abstract_rewriting)
- Nao Hirokawa and Aart Middeldorp. Decreasing diagrams and relative termination. *arXiv:0910.2853 [cs]*, October 2009. Comment: v3: missing references added. URL: <http://arxiv.org/abs/0910.2853> (visited on 2021-09-14), [arXiv:0910.2853](https://arxiv.org/abs/0910.2853).
- [TermComp](https://termcomp.herokuapp.com/Y2022/): TRS: [AProVE](https://aprove.informatik.rwth-aachen.de/references), matchbox, MnM, [MU-TERM](http://zenon.dsic.upv.es/muterm/index.php/documentation/), NaTT, NTI+cTI, TTT2 (Tyrolean Termination Tool 2); Higher-order: SOL, Wanda; C: irankfinder, LoAT; Complexity: tct_trs;
- [Rewrite engines competitions](https://web.archive.org/web/20200516055926/http://rec.gforge.inria.fr/)
- Comparing Curried and Uncurried Rewriting, author = {Kennaway, Richard and Klop, Jan Willem and Sleep, Ronan and Vries, Fer-jan De, 1995
- Mathematica's `Root` or `RootInterval` expressions
- Neil D. Jones and Nina Bohr. Call-by-value termination in the untyped lambda-calculus. *Logical Methods in Computer Science*, 4(1):3, March 2008. URL: <http://arxiv.org/abs/0801.0882> (visited on 2021-03-06), [arXiv:0801.0882](https://arxiv.org/abs/0801.0882), [doi:10.2168/LMCS-4(1:3)2008](https://doi.org/10.2168/LMCS-4(1:3)2008).
- https://en.wikipedia.org/wiki/Complete_metric_space#Completion
- https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
- http://conal.net/blog/posts/lazier-functional-programming-part-2
- http://augustss.blogspot.com/2011/05/more-points-for-lazy-evaluation-in.html
- Purely Functional Data Structures, Okasaki, Chris, 1998
- Jennifer Hackett and Graham Hutton. Call-by-need is clairvoyant call-by-value. *Proceedings of the ACM on Programming Languages*, 3(ICFP):1--23, July 2019. URL: <https://www.cs.nott.ac.uk/~pszgmh/clairvoyant.pdf> (visited on 2022-01-20), [doi:10.1145/3341718](https://doi.org/10.1145/3341718).
- http://reasonmlhub.com/exploring-reasonml/ch_functions.html
- https://hackage.haskell.org/package/base-4.9.0.0/docs/Text-Printf.html
- https://softwareengineering.stackexchange.com/questions/286231/is-it-possible-to-have-currying-and-variadic-function-at-the-same-time
- Adi Shamir. *The Fixedpoints of Recursive Definitions*. PhD thesis, Weizmann Institute of Science, Rehovot, October 1976. URL: <https://weizmann.primo.exlibrisgroup.com/permalink/972WIS_INST/1d4esio/alma990002185270203596>.
- https://github.com/wartremover/wartremover/issues/116
- Stefan Kahrs and Connor Smith. Non-omega-overlapping TRSs are UN. In Delia Kesner and Brigitte Pientka, editors, *1st International Conference on Formal Structures for Computation and Deduction (FSCD 2016)*, volume 52 of Leibniz International Proceedings in Informatics (LIPIcs), 22:1--22:17. Dagstuhl, Germany, 2016. Schloss Dagstuhl--Leibniz-Zentrum fuer Informatik. URL: <http://drops.dagstuhl.de/opus/volltexte/2016/5996> (visited on 2021-09-11), [doi:10.4230/LIPIcs.FSCD.2016.22](https://doi.org/10.4230/LIPIcs.FSCD.2016.22).
- https://www.reddit.com/r/ProgrammingLanguages/comments/jde9xp/advantages_of_currying_in_a_programming_language/
- https://www.swi-prolog.org/pack/list?p=func
- https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/PersistentDataStructure/00.11.36.jpg

**0.2.2 Symbol Tables & Name Resolution (Including Posets) #13**

- https://fedoraproject.org/wiki/Changes/PythonNoSemanticInterpositionSpeedup https://bugs.python.org/issue38980
- http://www.unicode.org/reports/tr39/#Confusable_Detection
- https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/code_and_name_organization/README.md#small-programs
- https://github.com/golang/go/issues/30247#issuecomment-463940936
- https://learn.microsoft.com/en-us/windows/apps/desktop/modernize/package-identity-overview
- https://semver.org/
- https://en.wikipedia.org/wiki/Partial_equivalence_relation
- https://grouper.ieee.org/groups/msc/ANSI_IEEE-Std-754-2019/background/predicates.txt
- https://arxiv.org/pdf/1909.07539.pdf
- https://github.com/FlowCrypt/idn-homographs-database
- https://arxiv.org/abs/2006.13742
- https://www.unicode.org/reports/tr31/tr31-37.html
- https://docs.oracle.com/javase/8/docs/api/java/text/RuleBasedCollator.html
- http://www.unicode.org/reports/tr31/#Table_Candidate_Characters_for_Exclusion_from_Identifiers
- {Trojan Source: Invisible Vulnerabilities}, {Boucher, Anderson},
- https://www.unicode.org/reports/tr44/#Character_Decomposition_Mappings
- https://unicode.org/reports/tr44/#Default_Ignorable_Code_Point
- https://www.unicode.org/Public/UCD/latest/ucd/DerivedCoreProperties.txt
- https://www.unicode.org/reports/tr39/#Confusable_Detection
- https://github.com/rust-lang/rfcs/blob/master/text/2457-non-ascii-idents.md
- https://www.unicode.org/Public/security/latest/confusables.txt
- https://github.com/rust-lang/rfcs/pull/2457#discussion_r192605996
- https://ystudios.com/insights-passion/codelanguage
- https://rosettacode.org/wiki/Unicode_variable_names
- https://marketplace.visualstudio.com/items?itemName=brunnerh.insert-unicode
- https://en.wikipedia.org/wiki/List_of_languages_by_total_number_of_speakers
- https://en.wikipedia.org/w/index.php?title=List_of_languages_by_total_number_of_speakers&direction=prev&oldid=1073408213
- https://en.wikipedia.org/wiki/Languages_used_on_the_Internet
- http://pinyin.info/readings/texts/moser.html
- https://en.wikipedia.org/wiki/Character_amnesia
- https://w3techs.com/technologies/segmentation/sl-cnter-/character_encoding
- https://web.archive.org/web/20041206080839/http://www.cs.fit.edu/~ryan/compress/
- http://www.unicode.org/faq/compression.html
- http://www.unicode.org/faq/normalization.html
- https://www.unicode.org/mail-arch/unicode-ml/y2003-m06/0423.html
- https://github.com/rust-lang/rfcs/pull/2457#issuecomment-395488644
- https://peps.python.org/pep-3131/
- https://github.com/golang/go/issues/27896
- https://groups.google.com/g/dev-python/c/LkCtik9LyyE/m/QcRz1gdfAQAJ
- https://www.unicode.org/reports/tr36/#Recommendations_General
- https://groups.google.com/g/dev-python/c/LkCtik9LyyE/m/ki8XN66iAQAJhttps://groups.google.com/g/dev-python/c/LkCtik9LyyE/m/ki8XN66iAQAJ
- https://stackoverflow.com/questions/48404881/unicode-subscripts-and-superscripts-in-identifiers-why-does-python-consider-xu
- https://mail.python.org/pipermail/python-3000/2007-May/007995.html
- https://www.w3.org/TR/charmod-norm/#canonical_compatibility
- http://unicode.org/faq/vs.html
- https://github.com/w3c/csswg-drafts/issues/1710
- https://gitlab.gnome.org/GNOME/pango/-/issues/206
- https://drafts.csswg.org/css-fonts-3/#cluster-matching
- https://github.com/harfbuzz/harfbuzz/issues/515#issuecomment-317932409
- https://docs.raku.org/language/syntax#Identifiers
- https://doc.rust-lang.org/rust-by-example/variable_bindings/scope.html
- https://web.archive.org/web/20250124213205/https://rules.sonarsource.com/cpp/RSPEC-1117/ Variables should not be shadowed


***

## Layer 1 – Deployed Image (Target‑Side)

**1.1 Tiered IR & Code Cache #14**

- https://youtu.be/gx8DVVFPkcQ?t=2171 picture of how JIT sees code
- Yunhe Shi, David Gregg, Andrew Beatty, and M. Anton Ertl. Virtual machine showdown: stack versus registers. In *Proceedings of the 1st ACM/USENIX International Conference on Virtual Execution Environments*, VEE '05, 153--163. New York, NY, USA, June 2005. Association for Computing Machinery. URL: <https://dl.acm.org/doi/10.1145/1328195.1328197> (visited on 2021-06-29), [doi:10.1145/1064979.1065001](https://doi.org/10.1145/1064979.1065001).
- Andrew W. Appel. SSA is functional programming. *ACM SIGPLAN Notices*, 33(4):17--20, April 1998. URL: <https://dl.acm.org/doi/10.1145/278283.278285> (visited on 2023-05-03), [doi:10.1145/278283.278285](https://doi.org/10.1145/278283.278285).
- Thorin
- https://mlir.llvm.org/docs/Rationale/Rationale/#block-arguments-vs-phi-nodes
- Chris Okasaki, Peter Lee, and David Tarditi. Call-by-need and continuation-passing style. *LISP and Symbolic Computation*, 7(1):57--81, January 1994. URL: <https://www.researchgate.net/profile/Peter-Lee-88/publication/220606923_Call-by-Need_and_Continuation-Passing_Style/links/55633a2108ae8c0cab3509ba/Call-by-Need-and-Continuation-Passing-Style.pdf> (visited on 2023-05-02), [doi:10.1007/BF01019945](https://doi.org/10.1007/BF01019945).
- Compiling without Continuations, {Maurer, Luke and Downen, Paul and Ariola, Zena M. and Peyton Jones, Simon}, {2017}, https://dl.acm.org/doi/10.1145/3062341.3062380
- Cormac Flanagan, Amr Sabry, Bruce F. Duba, and Matthias Felleisen. The essence of compiling with continuations. *ACM SIGPLAN Notices*, 28(6):237--247, June 1993. URL: <https://dl.acm.org/doi/10.1145/173262.155113> (visited on 2023-05-03), [doi:10.1145/173262.155113](https://doi.org/10.1145/173262.155113).
- Andrew Kennedy. Compiling with continuations, continued. In *Proceedings of the 12th ACM SIGPLAN International Conference on Functional Programming*, ICFP '07, 177--190. New York, NY, USA, October 2007. Association for Computing Machinery. URL: <https://doi.org/10.1145/1291151.1291179> (visited on 2023-05-03), [doi:10.1145/1291151.1291179](https://doi.org/10.1145/1291151.1291179).
- Amr Sabry and Matthias Felleisen. Reasoning about programs in continuation-passing style. In *Proceedings of the 1992 ACM Conference on LISP and Functional Programming*, LFP '92, 288--298. New York, NY, USA, January 1992. Association for Computing Machinery. URL: <https://dl.acm.org/doi/10.1145/141471.141563> (visited on 2023-04-21), [doi:10.1145/141471.141563](https://doi.org/10.1145/141471.141563).
- https://youtu.be/gx8DVVFPkcQ?t=2171
- Jack W. Davidson and Christopher W. Fraser. The design and application of a retargetable peephole optimizer. *ACM Transactions on Programming Languages and Systems*, 2(2):191--202, April 1980. URL: <https://dl.acm.org/doi/10.1145/357094.357098> (visited on 2021-07-30), [doi:10.1145/357094.357098](https://doi.org/10.1145/357094.357098).
- https://en.wikipedia.org/wiki/Tomasulo_algorithm
- https://internals.rust-lang.org/t/computed-gotos-tco-threaded-interpreters-experiments-and-findings/4668/6

**1.2 Stitching Logic & Guards/Deoptimization #15**

- https://github.com/ruby/ruby/blob/master/doc/yjit/yjit.md
- https://antocuni.eu/2025/10/29/inside-spy-part-1-motivations-and-goals/
- https://handbook.selflanguage.org/SelfHandbook2017.1.pdf
- https://github.com/brandtbucher/cpython/tree/justin/Tools/jit
- Simon Peyton Jones and Simon Marlow. Secrets of the Glasgow Haskell Compiler inliner. *Journal of Functional Programming*, 12(4):393--434, July 2002. URL: <https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf> (visited on 2020-07-01), [doi:10.1017/S0956796802004331](https://doi.org/10.1017/S0956796802004331).
- Simon Marlow and Simon Peyton Jones. Making a fast curry: push/enter vs. eval/apply for higher-order languages. *Journal of Functional Programming*, 16(4-5):415--449, July 2006. URL: <https://www.cambridge.org/core/product/identifier/S0956796806005995/type/journal_article> (visited on 2020-06-15), [doi:10.1017/S0956796806005995](https://doi.org/10.1017/S0956796806005995).
- Maximilian C. Bolingbroke and Simon L. Peyton Jones. Types are calling conventions. In *Proceedings of the 2nd ACM SIGPLAN Symposium on Haskell - Haskell '09*, 1. Edinburgh, Scotland, May 2009. ACM Press. URL: <https://www.microsoft.com/en-us/research/publication/types-are-calling-conventions/> (visited on 2020-06-15), [doi:10.1145/1596638.1596640](https://doi.org/10.1145/1596638.1596640).


Sources on JITs, speculative optimization, and guard semantics:

- LLVM and GCC optimization bugs and diagnostics; Intel/AMD microarchitectural quirks relevant to speculative code.

These support guard insertion, deoptimization points, and stitching optimized fragments into baseline code.

**1.3 Runtime Profiling, Instrumentation, & Baseline Behavior Estimator #16**

- https://stefan-marr.de/2025/10/can-we-know-whether-a-profiler-is-accurate/
- Scalability! But at what COST?
- https://blog.mozilla.org/jseward/2011/01/27/profiling-the-browsers-virtual-memory-behaviour/
- Young-Kyoon Suh, Richard T. Snodgrass, John D. Kececioglu, Peter J. Downey, Robert S. Maier, and Cheng Yi. EMP: execution time measurement protocol for compute-bound programs. *Software: Practice and Experience*, 47(4):559--597, 2017. URL: <https://onlinelibrary.wiley.com/doi/abs/10.1002/spe.2476> (visited on 2023-08-07), [doi:10.1002/spe.2476](https://doi.org/10.1002/spe.2476).
- David Snowdon, Godfrey Van Der Linden, Stefan Petters, and Gernot Heiser. Accurate run-time prediction of performance degradation under frequency scaling. In *2007 Workshop on Operating System Platforms for Embedded Real-Time Applications*. NICTA, 2007. URL: <http://hdl.handle.net/1959.4/39905> (visited on 2023-08-07), [doi:10.26190/unsworks/517](https://doi.org/10.26190/unsworks/517).
- https://github.com/andreas-abel/nanoBench
- https://github.com/sosy-lab/benchexec
- https://github.com/sharkdp/hyperfine
- https://github.com/parttimenerd/temci
- https://perf.wiki.kernel.org/
- http://www.lasid.ufba.br/publicacoes/artigos/Estimating+Execution+Time+Probability+Distributions+in+Component-based+Real-Time+Systems.pdf
- https://arxiv.org/pdf/2006.09864.pdf
- https://mjsaldanha.com/sci-projects/3-prob-exec-times-1/
-= Zixian Cai, Stephen M Blackburn, Michael D Bond, and Martin Maas. Distilling the real cost of production garbage collectors. *IEEE International Symposium on Performance Analysis of Systems and Software*, pages 12, 2022. URL: <https://users.cecs.anu.edu.au/~steveb/pubs/papers/lbo-ispass-2022.pdf>.

***

## Layer 2 – Host↔Target Protocol #17

- https://www.codeproject.com/Tips/430684/Loading-Win-DLLs-manually-without-LoadLibrary
- http://www.sco.com/developers/gabi/latest/ch5.pheader.html#p_type
- https://docs.microsoft.com/en-us/windows/win32/debug/pe-format
- https://gitlab.com/x86-psABIs/x86-64-ABI
- https://stackoverflow.com/questions/55607052/why-do-compilers-put-data-inside-textcode-section-of-the-pe-and-elf-files-and
- https://lists.llvm.org/pipermail/llvm-dev/2012-February/047555.html
- https://en.wikipedia.org/wiki/Literal_pool
- https://developer.android.com/reference/androidx/constraintlayout/widget/ConstraintLayout
- https://www.anandtech.com/show/3922/intels-sandy-bridge-architecture-exposed/3T
- https://www.mattkeeter.com/blog/2022-10-04-ssra/
- https://icculus.org/finger/icculus?date=2009-11-03&time=19-08-04
- http://golang.org/s/better-linker
- https://llvm.org/docs/PDB/index.html
- https://github.com/matheushjs/ElfProbTET
- https://microsoft.github.io/language-server-protocol/specification
- https://rust-analyzer.github.io/blog/2020/07/20/three-architectures-for-responsive-ide.html
- https://jupyter-client.readthedocs.io/en/latest/messaging.html

- Rust language server

These provide background for the protocol between host compiler and target runtime (binary payloads, debug info, and interactive control protocols).

***

## Layer 3 – Host‑Side Compiler

### 3.1 Abstract Interpreter #18

**3.1 Abstract Interpreter #18**

- Hashing {{Modulo Alpha-Equivalence}}, {Maziarz, Krzysztof and Ellis, Tom and Lawrence, Alan and Fitzgibbon, Andrew and Jones, Simon Peyton}, {2021},
- Laurent Mauborgne. *Representation of Sets of Trees for Abstract Interpretation*. PhD thesis, Ecole Polytechnique, November 1999. URL: <http://software.imdea.org/~mauborgn/publi/t.pdf>.
- Laurent Mauborgne. An incremental unique representation for regular trees. *Nordic Journal of Computing*, 7(4):290--311, December 2000.
- Maximilian Bolingbroke and Simon Peyton Jones. Supercompilation by evaluation. In *Proceedings of the Third ACM Haskell Symposium on Haskell*, Haskell '10, 135--146. New York, NY, USA, September 2010. Association for Computing Machinery. URL: <https://www.microsoft.com/en-us/research/publication/supercompilation-by-evaluation/> (visited on 2021-03-24), [doi:10.1145/1863523.1863540](https://doi.org/10.1145/1863523.1863540).
- Stephen Dolan and Alan Mycroft. Polymorphism, subtyping, and type inference in MLsub. In *Proceedings of the 44th ACM SIGPLAN Symposium on Principles of Programming Languages*, POPL 2017, 60--72. Paris, France, January 2017. Association for Computing Machinery. URL: <https://doi.org/10.1145/3009837.3009882> (visited on 2020-06-15), [doi:10.1145/3009837.3009882](https://doi.org/10.1145/3009837.3009882).
- D. L. Parnas, John E. Shore, and David Weiss. Abstract types defined as classes of variables. In *Proceedings of the 1976 Conference on Data : Abstraction, Definition and Structure*, 149--154. New York, NY, USA, March 1976. Association for Computing Machinery. URL: <https://dl.acm.org/doi/10.1145/800237.807133> (visited on 2023-08-29), [doi:10.1145/800237.807133](https://doi.org/10.1145/800237.807133).
- Stephen Dolan. *Algebraic Subtyping*. PhD thesis, University of Cambridge, September 2016. URL: <https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf>.
- J. Egger, R. E. Mogelberg, and A. Simpson. The enriched effect calculus: syntax and semantics. *Journal of Logic and Computation*, 24(3):615--654, June 2014. URL: <https://academic.oup.com/logcom/article-lookup/doi/10.1093/logcom/exs025> (visited on 2021-11-09), [doi:10.1093/logcom/exs025](https://doi.org/10.1093/logcom/exs025).
- Dylan McDermott and Alan Mycroft. Extended call-by-push-value: reasoning about effectful programs and evaluation order. In Luís Caires, editor, *Programming Languages and Systems*, volume 11423, pages 235--262. Springer International Publishing, Cham, 2019. URL: <http://link.springer.com/10.1007/978-3-030-17184-1_9> (visited on 2021-11-09), [doi:10.1007/978-3-030-17184-1_9](https://doi.org/10.1007/978-3-030-17184-1_9).
- Noam Zeilberger. *The Logical Basis of Evaluation Order and Pattern-Matching*. PhD thesis, Carnegie Mellon University, Pittsburgh, PA, April 2009. URL: <https://software.imdea.org/~noam.zeilberger/thesis.pdf>.
- Panagis Karazeris, Apostolos Matzaris, and Jiří Velebil. Final coalgebras in accessible categories. *Mathematical Structures in Computer Science*, 21(5):1067--1108, October 2011. URL: <https://www.cambridge.org/core/product/identifier/S0960129511000351/type/journal_article> (visited on 2021-03-11), [doi:10.1017/S0960129511000351](https://doi.org/10.1017/S0960129511000351).
- Janis Voigtländer. Asymptotic improvement of computations over free monads. In Philippe Audebaud and Christine Paulin-Mohring, editors, *Mathematics of Program Construction*, volume 5133, pages 388--403. Springer Berlin Heidelberg, Berlin, Heidelberg, 2008. URL: <https://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf> (visited on 2021-07-09), [doi:10.1007/978-3-540-70594-9_20](https://doi.org/10.1007/978-3-540-70594-9_20).
- https://cstheory.stackexchange.com/questions/48884/are-there-strongly-normalizing-lambda-terms-that-cannot-be-given-a-system-f-type
- Silvia Ghilezan. Strong normalization and typability with intersection types. *Notre Dame Journal of Formal Logic*, 37(1):44--52, January 1996. URL: <https://projecteuclid.org/journals/notre-dame-journal-of-formal-logic/volume-37/issue-1/Strong-Normalization-and-Typability-with-Intersection-Types/10.1305/ndjfl/1040067315.full> (visited on 2022-06-23), [doi:10.1305/ndjfl/1040067315](https://doi.org/10.1305/ndjfl/1040067315).
- Frank Pfenning. Church and Curry: combining intrinsic and extrinsic typing. In Christoph Benzmüller, Chad E. Brown, Jörg Siekmann, and Richard Statman, editors, *Reasoning in Simple Type Theory: Festschrift in Honour of Peter B. Andrews on His 70th Birthday*, volume 17 of Studies in Logic: Mathematical Logic and Foundations, pages 288--323. College Publications, London, December 2008. URL: <https://www.cs.cmu.edu/~fp/papers/andrews08.pdf>.
- https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Dynamic.html
- https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/
- https://en.wikipedia.org/wiki/Zero_one_infinity_rule
- Paul Downen, Zachary Sullivan, Zena M. Ariola, and Simon Peyton Jones. Making a faster curry with extensional types. In *Proceedings of the 12th ACM SIGPLAN International Symposium on Haskell*, Haskell 2019, 58--70. Berlin, Germany, August 2019. Association for Computing Machinery. URL: <https://www.microsoft.com/en-us/research/publication/making-a-faster-curry-with-extensional-types/> (visited on 2020-06-14), [doi:10.1145/3331545.3342594](https://doi.org/10.1145/3331545.3342594).
- https://web.archive.org/web/20260516082515/http://lambda-the-ultimate.org/node/2423


**3.1.1 Binding‑Time Analysis (BTA) #19**

- Giorgios Economopoulos, Paul Klint, and Jurgen Vinju. Faster scannerless GLR parsing. In Oege de Moor and Michael I. Schwartzbach, editors, *Compiler Construction*, volume 5501, pages 126--141. Springer Berlin Heidelberg, Berlin, Heidelberg, 2009. URL: <http://link.springer.com/10.1007/978-3-642-00722-4_10> (visited on 2020-06-15), [doi:10.1007/978-3-642-00722-4_10](https://doi.org/10.1007/978-3-642-00722-4_10).
- Matthew Might, David Darais, and Daniel Spiewak. Parsing with derivatives: a functional pearl. *ACM SIGPLAN Notices*, 46(9):189--195, September 2011. URL: <https://doi.org/10.1145/2034574.2034801> (visited on 2023-05-18), [doi:10.1145/2034574.2034801](https://doi.org/10.1145/2034574.2034801).
- {Parsing with First-Class Derivatives}, {Brachthauser, Jonathan Immanuel and Rendel, Tillmann and Ostermann, Klaus},
- Sebastian Erdweg, Tillmann Rendel, Christian Kästner, and Klaus Ostermann. Layout-sensitive generalized parsing. In David Hutchison, Takeo Kanade, Josef Kittler, Jon M. Kleinberg, Friedemann Mattern, John C. Mitchell, Moni Naor, Oscar Nierstrasz, C. Pandu Rangan, Bernhard Steffen, Madhu Sudan, Demetri Terzopoulos, Doug Tygar, Moshe Y. Vardi, Gerhard Weikum, Krzysztof Czarnecki, and Görel Hedin, editors, *Software Language Engineering*, volume 7745, pages 244--263. Springer Berlin Heidelberg, Berlin, Heidelberg, 2013. URL: <http://link.springer.com/10.1007/978-3-642-36089-3_14> (visited on 2020-06-15), [doi:10.1007/978-3-642-36089-3_14](https://doi.org/10.1007/978-3-642-36089-3_14).
- Michael D. Adams. Principled parsing for indentation-sensitive languages: revisiting Landin's offside rule. *ACM SIGPLAN Notices*, 48(1):511--522, January 2013. URL: <https://doi.org/10.1145/2480359.2429129> (visited on 2022-05-18), [doi:10.1145/2480359.2429129](https://doi.org/10.1145/2480359.2429129).
- Luís Eduardo de Souza Amorim, Michael J. Steindorfer, Sebastian Erdweg, and Eelco Visser. Declarative specification of indentation rules: a tooling perspective on parsing and pretty-printing layout-sensitive languages. In *Proceedings of the 11th ACM SIGPLAN International Conference on Software Language Engineering - SLE 2018*, 3--15. Boston, MA, USA, 2018. ACM Press. URL: <http://udesou.info/wp-content/uploads/2018/10/layout-pp.pdf> (visited on 2020-06-15), [doi:10.1145/3276604.3276607](https://doi.org/10.1145/3276604.3276607).
- S. Doaitse Swierstra. Combinator parsing: a short tutorial. In Ana Bove, Luís Soares Barbosa, Alberto Pardo, and Jorge Sousa Pinto, editors, *Language Engineering and Rigorous Software Development: International LerNet ALFA Summer School 2008, Piriapolis, Uruguay, February 24 - March 1, 2008, Revised Tutorial Lectures*, volume 5520 of Lecture Notes in Computer Science, pages 252--300. Springer, Berlin, Heidelberg, 2009. URL: <http://www.cs.uu.nl/research/techreps/repo/CS-2008/2008-044.pdf> (visited on 2023-05-18), [doi:10.1007/978-3-642-03153-3_6](https://doi.org/10.1007/978-3-642-03153-3_6).
- https://hackage.haskell.org/package/parsec-3.1.16.1/docs/Text-Parsec-Prim.html
- https://hackage.haskell.org/package/trifecta-2.1.2/docs/Text-Trifecta-Parser.html
- https://hackage.haskell.org/package/attoparsec-0.14.4/docs/Data-Attoparsec-Internal-Types.html#t:Parser
- https://hackage.haskell.org/package/megaparsec-9.3.0/docs/Text-Megaparsec-Internal.html#t:ParsecT
- https://github.com/ekmett/trifecta/issues/49#issuecomment-322073854
- Masaru Tomita. *Efficient Parsing for Natural Language*. Springer US, Boston, MA, 1986. ISBN 978-1-4419-5198-4 978-1-4757-1885-0. URL: <http://link.springer.com/10.1007/978-1-4757-1885-0> (visited on 2020-06-15), [doi:10.1007/978-1-4757-1885-0](https://doi.org/10.1007/978-1-4757-1885-0).
- Mark Johnson. The Computational Complexity of GLR Parsing. In Masaru Tomita, editor, *Generalized LR Parsing*, pages 35--42. Springer US, Boston, MA, 1991. URL: <https://doi.org/10.1007/978-1-4615-4034-2_3> (visited on 2023-05-22), [doi:10.1007/978-1-4615-4034-2_3](https://doi.org/10.1007/978-1-4615-4034-2_3).
- Sylvie Billot and Bernard Lang. The structure of shared forests in ambiguous parsing. In *Proceedings of the 27th Annual Meeting on Association for Computational Linguistics*, ACL '89, 143--151. USA, June 1989. Association for Computational Linguistics. URL: <https://dl.acm.org/doi/10.3115/981623.981641> (visited on 2023-05-22), [doi:10.3115/981623.981641](https://doi.org/10.3115/981623.981641).
- Elizabeth Scott. SPPF-Style parsing from Earley recognisers. *Electronic Notes in Theoretical Computer Science*, 203(2):53--67, April 2008. URL: <https://linkinghub.elsevier.com/retrieve/pii/S1571066108001497> (visited on 2020-06-15), [doi:10.1016/j.entcs.2008.03.044](https://doi.org/10.1016/j.entcs.2008.03.044).
- Paul Wankadia. Redgrep: from regular expression derivatives to LLVM. March 2013. URL: <https://www.youtube.com/watch?v=ZJOgDovsIsg> (visited on 2021-02-09).
- https://github.com/attresearch/yakker
- Michael Sperber and Peter Thiemann. Generation of LR parsers by partial evaluation. *ACM Transactions on Programming Languages and Systems (TOPLAS)*, 22(2):224--264, March 2000. URL: <http://dl.acm.org/doi/10.1145/349214.349219> (visited on 2020-06-15), [doi:10.1145/349214.349219](https://doi.org/10.1145/349214.349219).
- https://en.wikipedia.org/wiki/Equivalence_relation
- https://docs.raku.org/language/grammars.html
- https://mastodon.social/@zwarich@hachyderm.io/109559009711883166
- https://lptranslations.com/learn/how-accurate-is-google-translate/
- Eelco Visser. *Syntax Definition for Language Prototyping*. University, Amsterdam, 1997. ISBN 978-90-74795-75-3. Zugl.: Amsterdam, Univ., Diss., 1997.
- http://www.semdesigns.com/products/DMS/DMSParsers.html?Home=DMSLexers


Sources on partial evaluation and specialization:

- Generation of LR parsers by partial evaluation; parsing with derivatives and first‑class derivatives; scannerless GLR and layout‑sensitive parsing with declarative indentation rules.
- Syntax definition for language prototyping (Visser); DMS parsers; Earley/SPPF style parsing papers.

These embody static vs dynamic separation and binding‑time analysis for IR.

**3.1.2 Meta‑Programming and AOP #20**

- Shriram Krishnamurthi. *Linguistic Reuse*. PhD thesis, Rice University, 2001. URL: <https://scholarship.rice.edu/handle/1911/17993> (visited on 2021-08-06).
- John N Shutt. *Fexprs as the Basis of Lisp Function Application or \$vau : The Ultimate Abstraction*. PhD thesis, WORCESTER POLYTECHNIC INSTITUTE, August 2010. URL: <https://web.wpi.edu/Pubs/ETD/Available/etd-090110-124904/>.
- http://www.newlisp.org/index.cgi?page=Differences_to_Other_LISPs
- Oleg Kiselyov. How to write seemingly unhygienic and referentially opaque macros with syntax-rules. In *Third Workshop on Scheme and Functional Programming*, 77. October 2002. URL: <https://okmij.org/ftp/Scheme/Dirty-Macros.pdf>.
- Aspect oriented programming literature
- https://docs.scala-lang.org/tour/by-name-parameters.html


***

### 3.2 Verification & Logic Engine #21

**3.2 Verification & Logic Engine #21**

- P. Wadler and S. Blott. How to make ad-hoc polymorphism less ad hoc. In *Proceedings of the 16th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages - POPL '89*, 60--76. Austin, Texas, United States, 1989. ACM Press. URL: <http://portal.acm.org/citation.cfm?doid=75277.75283> (visited on 2022-06-07), [doi:10.1145/75277.75283](https://doi.org/10.1145/75277.75283).
- https://www.reddit.com/r/ProgrammingLanguages/comments/uynw2i/return_type_overloading_for_dynamic_languages/
- http://conal.net/blog/posts/merging-partial-values
- https://en.wikipedia.org/wiki/Domain_of_discourse
- Martin Odersky, Philip Wadler, and Martin Wehr. A second look at overloading. In *Proceedings of the Seventh International Conference on Functional Programming Languages and Computer Architecture - FPCA '95*, 135--146. La Jolla, California, United States, 1995. ACM Press. URL: <http://portal.acm.org/citation.cfm?doid=224164.224195> (visited on 2022-06-07), [doi:10.1145/224164.224195](https://doi.org/10.1145/224164.224195).
- https://web.archive.org/web/20200107071106/https://prime.haskell.org/wiki/Defaulting
- Curry, Verse, Flix, Oz
- Dale Miller. An overview of linear logic programming. In Thomas Ehrhard, Jean-Yves Girard, Paul Ruet, and Philip Scott, editors, *Linear Logic in Computer Science*, pages 119--150. Cambridge University Press, 1 edition, November 2004. URL: <https://www.cambridge.org/core/product/identifier/CBO9780511550850A013/type/book_part> (visited on 2021-04-02), [doi:10.1017/CBO9780511550850.004](https://doi.org/10.1017/CBO9780511550850.004).
- William E. Byrd. *Relational Programming in Minikanren: Techniques, Applications, and Implementations*. PhD thesis, Indiana University, August 2009. URL: <https://scholarworks.iu.edu/dspace/handle/2022/8777> (visited on 2022-07-14).
- Oleg Kiselyov, Chung-chieh Shan, Daniel P. Friedman, and Amr Sabry. Backtracking, interleaving, and terminating monad transformers: (functional pearl). In *Proceedings of the Tenth ACM SIGPLAN International Conference on Functional Programming - ICFP '05*, 192. Tallinn, Estonia, 2005. ACM Press. URL: <http://portal.acm.org/citation.cfm?doid=1086365.1086390> (visited on 2022-07-15), [doi:10.1145/1086365.1086390](https://doi.org/10.1145/1086365.1086390).
- https://www.amzi.com/articles/prolog_under_the_hood.htm
- http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/cut.html
- Fergus Henderson, Zoltan Somogyi, and Thomas Conway. Determinism analysis in the Mercury compiler. In *In Proceedings of the Australian Computer Science Conference*, 337--346. 1996. URL: <https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.17.3967&rep=rep1&type=pdf>.
- Paul Bone PhD "Automatic Parallelism in Mercury"
- Keith L. Clark. Negation as Failure. In Hervé Gallaire and Jack Minker, editors, *Logic and Data Bases*, pages 293--322. Springer US, Boston, MA, 1978. URL: <http://www.doc.ic.ac.uk/~klc/NegAsFailure.pdf> (visited on 2022-07-15), [doi:10.1007/978-1-4684-3384-5_11](https://doi.org/10.1007/978-1-4684-3384-5_11).
- https://www.mercurylang.org/information/doc-latest/mercury_ref/Modes.html#Modes
- https://stackoverflow.com/questions/28467011/what-are-the-main-technical-differences-between-prolog-and-minikanren-with-resp
- https://www.reddit.com/r/ProgrammingLanguages/comments/9kb9z5/logic_programming_languages/
- https://www.metalevel.at/sudoku/
- https://norvig.com/sudopy.shtml
- Prolog's Definite Clause Grammars
- https://www.swi-prolog.org/pldoc/doc/_SWI_/library/pio.pl
- http://xsb.sourceforge.net/manual1/manual1.pdf
- Kenneth E. Iverson. Notation as a tool of thought. *Communications of the ACM*, 23(8):444--465, August 1980. URL: <https://doi.org/10.1145/358896.358899> (visited on 2022-07-18), [doi:10.1145/358896.358899](https://doi.org/10.1145/358896.358899).
- https://www.richyhbm.co.uk/posts/method-overloading-in-go/
- https://github.com/golang/go/issues/21659#issuecomment-325382091
- https://github.com/Ixrec/rust-orphan-rules#what-is-coherence
- https://github.com/Ixrec/rust-orphan-rules#whats-wrong-with-incoherence
- https://www.reddit.com/r/haskell/comments/mprk2e/generalized_named_and_exportable_default/gubpfbn/
- https://ghc-proposals.readthedocs.io/en/latest/proposals/0409-exportable-named-default.html
- https://github.com/hasktorch/hasktorch/issues/514
- https://github.com/JuliaLang/julia/issues/19206
- https://artificial-mind.net/blog/2020/10/10/return-type-overloading
- https://pcarleton.com/2021/04/28/rust-what-to-pick-for-the-type-when-writing-your-own-result-type/

Jean-Yves Girard. Locus Solum: From the rules of logic to the logic of rules. *Mathematical Structures in Computer Science*, 11(3):301--506, June 2001. URL: <https://www.cambridge.org/core/journals/mathematical-structures-in-computer-science/article/abs/locus-solum-from-the-rules-of-logic-to-the-logic-of-rules/6318E18EA633F9692D9CDBA9DE4438C9> (visited on 2022-03-27), [doi:10.1017/S096012950100336X](https://doi.org/10.1017/S096012950100336X).
Vivek Nigam and Dale Miller. Algorithmic specifications in linear logic with subexponentials. In *Proceedings of the 11th ACM SIGPLAN Conference on Principles and Practice of Declarative Programming - PPDP '09*, 129. Coimbra, Portugal, 2009. ACM Press. URL: <http://portal.acm.org/citation.cfm?doid=1599410.1599427> (visited on 2021-03-26), [doi:10.1145/1599410.1599427](https://doi.org/10.1145/1599410.1599427).
- https://en.wikipedia.org/wiki/Structured_program_theorem#Implications_and_refinements
- https://news.ycombinator.com/item?id=14439137


**3.2.1 CEGAR & Predicate Abstraction #22**

- Vijay D'Silva and Caterina Urban. Conflict-driven conditional termination. In Daniel Kroening and Corina S. Păsăreanu, editors, *Computer Aided Verification*, Lecture Notes in Computer Science, 271--286. Cham, 2015. Springer International Publishing. URL: <https://link.springer.com/content/pdf/10.1007%2F978-3-319-21668-3_16.pdf>, [doi:10.1007/978-3-319-21668-3_16](https://doi.org/10.1007/978-3-319-21668-3_16).
- CPAChecker algorithm
- modal μ-calculus
- Franz Brauße, Konstantin Korovin, Margarita Korovina, and Norbert Th Müller. A CDCL-style calculus for solving non-linear constraints. *arXiv:1905.09227 [cs]*, July 2019. Comment: 17 pages, 3 figures; accepted at FroCoS 2019; software available at $<$http://informatik.uni-trier.de/~brausse/ksmt/$>$. URL: <http://arxiv.org/abs/1905.09227> (visited on 2020-07-25), [arXiv:1905.09227](https://arxiv.org/abs/1905.09227).
- Elvira Albert, Miquel Bofill, Cristina Borralleras, Enrique Martin-Martin, and Albert Rubio. Resource analysis driven by (conditional) termination proofs. *Theory and Practice of Logic Programming*, 19(5-6):722--739, September 2019. Comment: Paper presented at the 35th International Conference on Logic Programming (ICLP 2019), Las Cruces, New Mexico, USA, 20-25 September 2019, 16 pages. URL: <http://arxiv.org/abs/1907.10096> (visited on 2020-06-22), [arXiv:1907.10096](https://arxiv.org/abs/1907.10096), [doi:10.1017/S1471068419000152](https://doi.org/10.1017/S1471068419000152).
- http://mmjb.github.io/T2/

Sources directly on conditional termination and abstraction:

- conditional termination and SMT‑driven analyses.

**3.2.2 SMT/SAT & Logic Engine for Verification #23**

- Jason Hemann and Daniel P. . Friedman. microKanren: a minimal functional core for relational programming. In *Proceedings of the 2013 Workshop on Scheme and Functional Programming*. 2013. URL: <http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf> (visited on 2022-07-14).
- Jason Hemann and Daniel P. Friedman. A framework for extending microkanren with constraints. *Electronic Proceedings in Theoretical Computer Science*, 234:135--149, January 2017. Comment: In Proceedings WLP'15/'16/WFLP'16, arXiv:1701.00148. URL: <http://arxiv.org/abs/1701.00633> (visited on 2022-07-15), [arXiv:1701.00633](https://arxiv.org/abs/1701.00633), [doi:10.4204/EPTCS.234.10](https://doi.org/10.4204/EPTCS.234.10).

- model checkers: [CBMC](https://www.cprover.org/cbmc/), Verdi, Ironfleet, JSCert, Cosette, FSCQ, Chapar, CertiKOS, Linksem, miTLS and HACL\*, Versat and IsaSAT, CakeML
- Concolic (Concrete-Symbolic) testing or dynamic symbolic execution: DART, CUTE, KLEE, [NASA’s Java Pathfinder](https://github.com/javapathfinder), jCUTE, SAGE
- SV-COMP:
  > - Overall: [2LS](https://github.com/diffblue/2ls), [CBMC/CProver-witness2test](https://www.cprover.org/cbmc/), [CoVeriTeam-Verifier (AlgoSelection, ParallelPortfolio)](https://gitlab.com/sosy-lab/software/coveriteam), [CPAchecker, CPALockator, CPA-BAM-BnB, CPA-BAM-SMG, CPA-witness2test](https://cpachecker.sosy-lab.org), [DIVINE](https://divine.fi.muni.cz/), [ESBMC-kind, ESBMC-incr](https://esbmc.org/), [Goblint](https://goblint.in.tum.de/), [Symbiotic, Symbiotic-Witch](https://github.com/staticafi/symbiotic), [UAutomizer, UGemCutter, UKojak, UTaipan](https://ultimate.informatik.uni-freiburg.de), [VeriFuzz/VeriAbs](https://www.tcs.com/designing-complex-intelligent-systems)
  > - Concurrency: [CSeq](https://gitlab.com/emersonwds/cseq), [Lazy-CSeq](https://github.com/omainv/cseq/releases), [Dartagnan](https://github.com/hernanponcedeleon/Dat3M), [Deagle](https://github.com/thufv/Deagle), [EBF](https://github.com/fatimahkj/EBF), [Locksmith](http://www.cs.umd.edu/projects/PL/locksmith/)
  > - Java: [COASTAL](https://www.cs.sun.ac.za/coastal), [GDart](https://github.com/tudo-aqua/gdart-svcomp), [Java-Ranger](https://github.com/vaibhavbsharma/java-ranger), [JayHorn](https://github.com/jayhorn/jayhorn), [JBMC](https://github.com/diffblue/cbmc), [JDart](https://github.com/tudo-aqua/jdart), [SPF](https://github.com/SymbolicPathFinder/jpf-symbc)
  > - Reachability: [BRICK](https://github.com/brick-tool-dev/BRICK-2.0), [Crux](https://crux.galois.com/), [Theta](https://github.com/ftsrg/theta), [Gazer-Theta](https://github.com/ftsrg/gazer), [Graves-CPA](https://github.com/will-leeson/cpachecker), [LART](https://github.com/xlauko/lart), [Pinaka](https://github.com/sbjoshi/Pinaka), [PeSCo](https://github.com/cedricrupb/cpachecker)
  > - Overflow checking: [Frama-C-SV](https://gitlab.com/sosy-lab/software/frama-c-sv), [Infer](https://fbinfer.com/)
  > - Loop invariants: [Korn](https://github.com/gernst/korn)
  > - Memory safety: [PredatorHP](https://www.fit.vutbr.cz/research/groups/verifit/tools/predator-hp/), [SESL](https://spencerl-y.github.io/SESL/)
  > - Software systems: [SMACK](https://smackers.github.io/)

http://project-coco.uibk.ac.at/2023/

**3.2.3 Verification Meta‑techniques & Portfolios #24**

- ties in with #23
- https://en.wikipedia.org/wiki/Halting_problem#Approximations
- Marie-Christine Jakobs. Differential modular software verification. URL: <https://www.sosy-lab.org/research/prs/2019-10-01-CPA19-Differntial-Verification.pdf> (visited on 2020-07-25).
- https://en.wikipedia.org/wiki/Complete_partial_order
- https://www.cis.upenn.edu/~mhnaik/papers/toplas08.pdf

Sources on portfolios and competitive verification:

- Termination Competition and TermComp; CoCo project

**3.2.4 Invariant Checker & Adapter Layer #25**

- https://gitlab.haskell.org/ghc/ghc/issues/2273


Sources on invariants and abstract domains:

- Resource analysis for logic programs; mode and determinism analysis in Mercury; abstract domains for term rewriting strategies.

All of these collectively support the design of a verification engine layered over your IR.

***

### 3.3 Optimizing Compiler #26

**3.3 Optimizing Compiler #26**

- http://venge.net/graydon/talks/CompilerTalk-2019.pdf
- Frances E Allen and John Cocke. A catalogue of optimizing transformations. *IBM Research Center*, pages 30, 1971. URL: <https://www.clear.rice.edu/comp512/Lectures/Papers/1971-allen-catalog.pdf>.
- https://en.wikipedia.org/wiki/Wide-spectrum_language
- Richard Bird, Geraint Jones, and Oege De Moor. More haste, less speed: lazy versus eager evaluation. *Journal of Functional Programming*, 7(5):541--547, September 1997. URL: <https://www.cambridge.org/core/journals/journal-of-functional-programming/article/more-haste-less-speed-lazy-versus-eager-evaluation/162B391CBCD864794C766CA2A2EC7CBE> (visited on 2022-01-06), [doi:10.1017/S0956796897002827](https://doi.org/10.1017/S0956796897002827).
- Aviral Goel and Jan Vitek. On the design, implementation, and use of laziness in R. *Proceedings of the ACM on Programming Languages*, 3(OOPSLA):1--27, October 2019. Comment: 27 pages, 4 tables, 21 figures. URL: <http://arxiv.org/abs/1909.08958> (visited on 2022-10-25), [arXiv:1909.08958](https://arxiv.org/abs/1909.08958), [doi:10.1145/3360579](https://doi.org/10.1145/3360579).
- https://bellard.org/otcc/
- https://www.youtube.com/watch?v=p5S1K60mhQU
- https://hal.inria.fr/inria-00072269/document The Treewidth of Java Programs


**3.3.1 IR Normalization & Cleanup Passes #27**

- https://en.wikipedia.org/wiki/Tree_shaking
- https://pp.ipd.kit.edu/uploads/publikationen/graf19sll.pdf

**3.3.2 Tunable Optimization Objective Engine #28**

- https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81616
- https://reviews.llvm.org/D118534
- https://www.rdocumentation.org/packages/evd/versions/2.3-6/topics/gev


- Papers on performance of garbage collectors; cost of address translation; execution‑time distribution estimation; performance prediction under frequency scaling.
- compiler flag tuning discussions.

These inform cost models and multi‑objective optimization engines.

**3.3.3 Unison‑Style Global Code Generator (Compiler Core) #29**

- https://dl.acm.org/doi/abs/10.1145/143103.143142


- Retargetable peephole optimizer; cross‑compilation docs; calling conventions for system calls; ABI specifications (System V, Swift ABI, x86‑64 ABI).
- Libffi, `rust-bindgen`, `dragonffi`, foreign‑function tooling.

These are relevant to global scheduling, register allocation, and ABI‑aware code generation.

**3.3.4 Tracing & Loop Analysis #30**

- Structured program theorem and refinements; loop nesting and semantics; compilation of control structures.
- Work on CPS vs direct style and tracing of control flow (Flanagan/Sabry/Felleisen, Kennedy "compiling with continuations, continued").

***

### 3.4 Tier Controller #31

**3.4 Tier Controller #31**

- Discussions of compiler “ways” in GHC RTS; Rustc developer guide’s overview and compiler pipelines; Rust compiler architecture blog posts and talks.
- Stage0‑posix, diverse double‑compiling (Wheeler), bootstrapping sequences.

**3.4.1 Incremental/Parallel Build Engine #32**

- Rustc build internals; Zig build system and internals; Java VM build/packaging whitepapers; build‑system descriptions for GHC.

These inform multi‑tier compilation, incremental builds, and cross‑compilation orchestration.

***

## Layer 4 – Bootstrapping #33

**4.4 Self‑Hosting & Cross‑Compilation #34**

- https://clang.llvm.org/docs/CrossCompilation.html
- https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html
- David A. Wheeler. Fully countering trusting trust through diverse double-compiling. *arXiv:1004.5534 [cs]*, April 2010. Comment: PhD dissertation. Accepted by George Mason University, Fairfax, Virginia, USA's Volgenau School of Information Technology and Engineering in 2009. 199 single-side printed pages. URL: <http://arxiv.org/abs/1004.5534> (visited on 2020-09-13), [arXiv:1004.5534](https://arxiv.org/abs/1004.5534).
- https://github.com/oriansj/stage0-posix
- https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/compiler-ways
- Jean-Paul Tremblay and Paul G. Sorenson. *The Theory and Practice of Compiler Writing*. McGraw-Hill College, New York, first edition edition, January 1985. ISBN 978-0-07-065161-6.
- https://users.rust-lang.org/t/understanding-how-the-rust-compiler-is-built/87237
- https://mitchellh.com/zig/build-internals
- https://aosabook.org/en/v2/ghc.html
- https://tomlee.co/2014/04/a-more-detailed-tour-of-the-rust-compiler/
- https://rustc-dev-guide.rust-lang.org/compiler-src.html

**4.5 Build System #35**

- https://ziglang.org/learn/build-system/
- GHC build system docs; Rustc dev guide sections on building from source.

These are directly tied to your bootstrapping plan.

***

## Layer 5 – Developer Tooling & Meta

**5.1 Interactive Execution & Debugger #36**

- ties in with host-target protocol #17
- http://blog.ezyang.com/2011/05/anatomy-of-a-thunk-leak/
- https://survey.stackoverflow.co/2022/#section-most-popular-technologies-integrated-development-environment
- https://petevilter.me/post/datalog-typechecking/
- https://go.dev/blog/gofmt
- https://github.com/golang/go/issues/40028
- https://www.reddit.com/r/golang/comments/ee4dqn/how_to_make_gofmt_or_go_fmt_use_whitespaces/
- https://www.reddit.com/r/golang/comments/rrce2e/is_there_a_better_alternative_to_gofmt/hqflh4i/
- https://www.reddit.com/r/golang/comments/rt0hra/can_i_tell_gofmt_to_make_some_exceptions_when/
- https://github.com/llvm/llvm-project/issues/59729
- https://www.youtube.com/watch?v=WxyyJyB_Ssc
- https://github.com/haskell/haddock/commit/d300632cbc2346f6d95188426e5db5fbeb7c9f34

**5.2 Test Suite #37**

- ties in with profiling in #16
- https://www.nngroup.com/articles/response-times-3-important-limits/
- use pytest, also look at gtest and go test
- https://counterexamples.org/under-construction.html
- https://www.mend.io/blog/is-one-programming-language-more-secure/
- https://www.cvedetails.com/vulnerability-list/vendor_id-7252/product_id-12215/Ruby-lang-Ruby.html
- https://harelang.org/blog/2022-06-21-safety-features/
- https://cwe.mitre.org/data/index.html
- https://github.com/OWASP/ASVS/blob/d8fde8b6592af2b8022590ec9d9a1765fe920651/4.0/docs_en/OWASP%20Application%20Security%20Verification%20Standard%204.0.3-en.csv
- https://github.com/OWASP/ASVS/tree/d8fde8b6592af2b8022590ec9d9a1765fe920651/5.0/en
- https://github.com/cliffclick/aa/issues/28
- https://github.com/gelisam/circular-sig
- https://github.com/UlfNorell/insane/blob/master/Context.agda
- https://github.com/UlfNorell/insane/
- https://ecommons.cornell.edu/bitstream/handle/1813/34898/bbang.pdf?sequence=2


**5.3 Compiler Feature Flags & Diagnostic System #38**

- https://elm-lang.org/news/compiler-errors-for-humans
- Brett A. Becker, Paul Denny, Raymond Pettit, Durell Bouchard, Dennis J. Bouvier, Brian Harrington, Amir Kamil, Amey Karkare, Chris McDonald, Peter-Michael Osera, Janice L. Pearce, and James Prather. Compiler error messages considered unhelpful: The landscape of text-based programming error message research. In *Proceedings of the Working Group Reports on Innovation and Technology in Computer Science Education*, 177--210. Aberdeen Scotland Uk, December 2019. ACM. URL: <https://dl.acm.org/doi/10.1145/3344429.3372508> (visited on 2021-03-30), [doi:10.1145/3344429.3372508](https://doi.org/10.1145/3344429.3372508).
- https://www.oreilly.com/library/view/designed-for-use/9781680501902/f_0298.xhtml
- https://faculty.washington.edu/ajko/papers/Lee2011Gidget.pdf
- https://www.codewithjason.com/whos-blame-bad-code-coders/
- https://github.com/elm/error-message-catalog
- https://doc.rust-lang.org/error-index.html
- Davin McCall and Michael Kölling. A new look at novice programmer errors. *ACM Transactions on Computing Education*, 19(4):1--30, December 2019. URL: <https://dl.acm.org/doi/10.1145/3335814> (visited on 2022-10-15), [doi:10.1145/3335814](https://doi.org/10.1145/3335814).
- Martin Monperrus. Explainable software bot contributions: case study of automated bug fixes. In *2019 IEEE/ACM 1st International Workshop on Bots in Software Engineering (BotSE)*, 12--15. May 2019. URL: <http://arxiv.org/abs/1905.02597> (visited on 2023-06-19), [arXiv:1905.02597](https://arxiv.org/abs/1905.02597), [doi:10.1109/BotSE.2019.00010](https://doi.org/10.1109/BotSE.2019.00010).
- Titus Barik, Denae Ford, Emerson Murphy-Hill, and Chris Parnin. How should compilers explain problems to developers? In *Proceedings of the 2018 26th ACM Joint Meeting on European Software Engineering Conference and Symposium on the Foundations of Software Engineering*, 633--643. Lake Buena Vista FL USA, October 2018. ACM. URL: <https://dl.acm.org/doi/10.1145/3236024.3236040> (visited on 2022-10-15), [doi:10.1145/3236024.3236040](https://doi.org/10.1145/3236024.3236040).
- Titus Barik. *Error Messages as Rational Reconstructions*. PhD thesis, North Carolina State University, 2018.
- https://style.tidyverse.org/error-messages.html
- https://github.com/microsoft/vscode/issues/14165
- https://stackoverflow.com/questions/44638328/vs-code-size-of-description-popup
- https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Pragmas.html#Diagnostic-Pragmas

**5.4 Standard Library #39**

- Simon Peyton Jones. Tackling the awkward squad: monadic input/output, concurrency, exceptions, and foreign-language calls in Haskell. In *Engineering Theories of Software Construction*, 47--96. 2001. URL: <https://www.microsoft.com/en-us/research/publication/tackling-awkward-squad-monadic-inputoutput-concurrency-exceptions-foreign-language-calls-haskell/>.
- https://www.reddit.com/r/haskell/comments/swffy/why_do_we_not_define_io_as_a_free_monad/
- http://comonad.com/reader/2011/free-monads-for-less-3/
- https://hackage.haskell.org/package/pure-io-0.2.1/docs/PureIO.html
- https://www.mercurylang.org/information/doc-latest/mercury_trans_guide/IO.html
- {On the {{Expressiveness}} of {{Purely Functional I/O Systems}}},Hudak, Paul and Sundaresh, Raman S, 1989
- https://en.wikipedia.org/wiki/Ada_(programming_language)#History
- https://www.artima.com/articles/the-philosophy-of-ruby
- https://www.adahome.com/History/Steelman/steelman.htm
- http://iment.com/maida/computer/requirements/tinman.htm
- A Common Programming Language for the {{Department}} of {{Defense--Background}} and Technical Requirements, {Fisher, David A.}, {1976}, {https://apps.dtic.mil/sti/citations/ADA028297},
- http://www.ada-auth.org/standards/ada22.html
- https://dart.dev/guides/language/spec
- https://wiki.c2.com/?WhyWeHateCobol
- Alan Snyder. Encapsulation and inheritance in object-oriented programming languages. *ACM SIGPLAN Notices*, 21(11):38--45, November 1986. URL: <https://dl.acm.org/doi/10.1145/960112.28702> (visited on 2022-12-26), [doi:10.1145/960112.28702](https://doi.org/10.1145/960112.28702).
- Kristen Nygaard and Ole-Johan Dahl. The development of the SIMULA languages. In *History of Programming Languages*, pages 439--480. Association for Computing Machinery, New York, NY, USA, June 1978. URL: <https://doi.org/10.1145/800025.1198392> (visited on 2022-12-20).
- https://catern.com/inheritance.html
- https://www.youtube.com/watch?v=xcpSLRpOMJM
- https://blog.berniesumption.com/software/inheritance-is-evil-and-must-be-destroyed/index.html
- K. El Emam, S. Benlarbi, N. Goel, and S.N. Rai. The confounding effect of class size on the validity of object-oriented metrics. *IEEE Transactions on Software Engineering*, 27(7):630--650, July 2001. URL: <http://ieeexplore.ieee.org/document/935855/> (visited on 2023-02-24), [doi:10.1109/32.935855](https://doi.org/10.1109/32.935855).
- William R Cook. *A Denotational Semantics of Inheritance*. PhD thesis, Brown University, May 1989. URL: <https://www.cs.utexas.edu/~wcook/papers/thesis/cook89.pdf>.
- taivalsaariNotionInheritance19Antero Taivalsaari. On the notion of inheritance. *ACM Computing Surveys*, 28(3):438--479, September 1996. URL: <https://doi.org/10.1145/243439.243441> (visited on 2022-12-14), [doi:10.1145/243439.243441](https://doi.org/10.1145/243439.243441).96
- https://github.com/NixOS/nixpkgs/blob/d44a67c4ba1a01614f236213b3f64e17bb107879/lib/fixed-points.nix#L91
- https://github.com/Mathnerd314/stroscot/blob/master/library/inheritance.txt
- https://journal.stuffwithstuff.com/2012/12/19/the-impoliteness-of-overriding-methods/
- Ewan Tempero, Hong Yul Yang, and James Noble. What programmers do with inheritance in Java. In David Hutchison, Takeo Kanade, Josef Kittler, Jon M. Kleinberg, Friedemann Mattern, John C. Mitchell, Moni Naor, Oscar Nierstrasz, C. Pandu Rangan, Bernhard Steffen, Madhu Sudan, Demetri Terzopoulos, Doug Tygar, Moshe Y. Vardi, Gerhard Weikum, and Giuseppe Castagna, editors, *ECOOP 2013 -- Object-Oriented Programming*, volume 7920, pages 577--601. Springer Berlin Heidelberg, Berlin, Heidelberg, 2013. URL: <http://link.springer.com/10.1007/978-3-642-39038-8_24> (visited on 2022-08-31), [doi:10.1007/978-3-642-39038-8_24](https://doi.org/10.1007/978-3-642-39038-8_24).
- William R. Cook, Walter Hill, and Peter S. Canning. Inheritance is not subtyping. In *Proceedings of the 17th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages*, POPL '90, 125--135. New York, NY, USA, December 1989. Association for Computing Machinery. URL: <https://doi.org/10.1145/96709.96721> (visited on 2022-12-15), [doi:10.1145/96709.96721](https://doi.org/10.1145/96709.96721).
- Moez AbdelGawad and Robert Cartwright. NOOP: a domain-theoretic model of nominally-typed OOP. January 2018. Comment: 23 pages. URL: <http://arxiv.org/abs/1801.06793> (visited on 2022-12-27), [arXiv:1801.06793](https://arxiv.org/abs/1801.06793).
- https://codeblog.jonskeet.uk/2006/03/04/inheritancetax/
- https://en.wikipedia.org/w/index.php?diff=580604701
- Ole-Johan Dahl, Bjørn Myhrhaug, and Kristen Nygaard. Common Base Language. October 1970. URL: <https://www.ics.uci.edu/~jajones/INF102-S18/readings/10_Simula.pdf> (visited on 2022-12-20).
- https://blog.cleancoder.com/uncle-bob/2018/04/13/FPvsOO.html
- Artima - Java Design Issues. URL: <https://www.artima.com/articles/java-design-issues> (visited on 2022-12-13).
- https://www.artima.com/articles/how-to-write-an-equality-method-in-java
- https://docs.idris-lang.org/en/latest/tutorial/interfaces.html#named-implementations
- Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides, and Grady Booch. *Design Patterns: Elements of Reusable Object-Oriented Software*. Addison-Wesley Professional, Reading, Mass, 1st edition edition, October 1994. ISBN 978-0-201-63361-0.
- Larry O'Brien. Design patterns 15 years later: an interview. URL: <https://www.informit.com/articles/article.aspx?p=1404056> (visited on 2023-07-26).
- https://en.wikipedia.org/w/index.php?title=Template:Design_patterns&oldid=1032062304
- https://en.wikipedia.org/wiki/Software_design_pattern#Criticism
- https://wiki.c2.com/?DesignPatternsAreMissingLanguageFeatures
- {Design Patterns in Dynamic Programming}, {Norvig, Peter}, {1996}
- Mike O'Docherty. *Object-Oriented Analysis and Design: Understanding System Development with UML 2.0*. Wiley, Chichester, England ; Hoboken, NJ, 2005. ISBN 978-0-470-09240-8.
https://blog.mozilla.org/metrics/2009/08/11/how-many-firefox-users-customize-their-browser/
https://en.wikipedia.org/wiki/The_Humane_Interface
https://en.wikipedia.org/wiki/Principle_of_least_astonishment
https://www.youtube.com/watch?v=f3rP3JRq7Mw
http://rigaux.org/language-study/syntax-across-languages/
https://rosettacode.org/wiki/Comments
Andreas Stefik and Susanna Siebert. An empirical investigation into programming language syntax. *ACM Transactions on Computing Education*, 13(4):1--40, November 2013. URL: <https://dl.acm.org/doi/10.1145/2534973> (visited on 2021-02-13), [doi:10.1145/2534973](https://doi.org/10.1145/2534973).
Rodrigo Magalhães dos Santos and Marco Aurélio Gerosa. Impacts of coding practices on readability. In *Proceedings of the 26th Conference on Program Comprehension*, 277--285. Gothenburg Sweden, May 2018. ACM. URL: <https://dl.acm.org/doi/10.1145/3196321.3196342> (visited on 2022-05-18), [doi:10.1145/3196321.3196342](https://doi.org/10.1145/3196321.3196342).
https://insights.stackoverflow.com/survey/2021
Janet Siegmund, Christian Kästner, Jörg Liebig, Sven Apel, and Stefan Hanenberg. Measuring and modeling programming experience. *Empirical Software Engineering*, 19(5):1299--1334, October 2014. URL: <https://doi.org/10.1007/s10664-013-9286-4> (visited on 2023-02-13), [doi:10.1007/s10664-013-9286-4](https://doi.org/10.1007/s10664-013-9286-4).

https://www.kinvert.com/age-teach-kids-python/
https://www.economist.com/johnson/2013/05/29/lexical-facts
Allison Elliott Tew and Mark Guzdial. Developing a validated assessment of fundamental CS1 concepts. In *Proceedings of the 41st ACM Technical Symposium on Computer Science Education*, 97--101. Milwaukee Wisconsin USA, March 2010. ACM. URL: <https://dl.acm.org/doi/10.1145/1734263.1734297> (visited on 2023-02-13), [doi:10.1145/1734263.1734297](https://doi.org/10.1145/1734263.1734297).
Allison Elliott Tew and Mark Guzdial. The FCS1: a language independent assessment of CS1 knowledge. In *Proceedings of the 42nd ACM Technical Symposium on Computer Science Education*, 111--116. Dallas TX USA, March 2011. ACM. URL: <https://dl.acm.org/doi/10.1145/1953163.1953200> (visited on 2023-02-13), [doi:10.1145/1953163.1953200](https://doi.org/10.1145/1953163.1953200).
http://www.lamdu.org/
https://www.rtings.com/monitor/reviews/best/by-usage/programming-and-coding
https://en.wikipedia.org/wiki/Pixel_density#Printing_on_paper
https://store.steampowered.com/hwsurvey/Steam-Hardware-Software-Survey-Welcome-to-Steam
https://nickjanetakis.com/blog/how-to-pick-a-good-monitor-for-software-development
Sofie Beier. *Typeface Legibility: Towards Defining Familiarity*. Thesis, Royal College of Art, May 2009. URL: <https://researchonline.rca.ac.uk/957/> (visited on 2023-02-11).
Peter Walker. Font tuning: A review and new experimental evidence. *Visual Cognition*, 16(8):1022--1058, November 2008. URL: <http://www.tandfonline.com/doi/abs/10.1080/13506280701535924> (visited on 2023-01-13), [doi:10.1080/13506280701535924](https://doi.org/10.1080/13506280701535924).
Sofie Beier and Kevin Larson. How does typeface familiarity affect reading performance and reader preference? *Information Design Journal*, 20(1):16--31, October 2013. URL: <http://www.jbe-platform.com/content/journals/10.1075/idj.20.1.02bei> (visited on 2023-01-13), [doi:10.1075/idj.20.1.02bei](https://doi.org/10.1075/idj.20.1.02bei).
Jennifer Bauer, Janet Siegmund, Norman Peitek, Johannes C. Hofmeister, and Sven Apel. Indentation: simply a matter of style or support for program comprehension? In *2019 IEEE/ACM 27th International Conference on Program Comprehension (ICPC)*, 154--164. Montreal, QC, Canada, May 2019. IEEE. URL: <https://ieeexplore.ieee.org/document/8813302/> (visited on 2022-05-19), [doi:10.1109/ICPC.2019.00033](https://doi.org/10.1109/ICPC.2019.00033).
Raymond P.L. Buse and Westley R. Weimer. A metric for software readability. In *Proceedings of the 2008 International Symposium on Software Testing and Analysis - ISSTA '08*, 121. Seattle, WA, USA, 2008. ACM Press. URL: <http://portal.acm.org/citation.cfm?doid=1390630.1390647> (visited on 2022-05-19), [doi:10.1145/1390630.1390647](https://doi.org/10.1145/1390630.1390647).
Aries Arditi, Kenneth Knoblauch, and Ilana Grunwald. Reading with fixed and variable character pitch. *Journal of the Optical Society of America A*, 7(10):2011, October 1990. URL: <https://opg.optica.org/abstract.cfm?URI=josaa-7-10-2011> (visited on 2022-05-25), [doi:10.1364/JOSAA.7.002011](https://doi.org/10.1364/JOSAA.7.002011).
https://www.w3.org/TR/CSS2/tables.html#auto-table-layout
https://github.com/isral/elastic_tabstops_mono.vsce
https://blog.codinghorror.com/we-are-typists-first-programmers-second/
https://www.stroustrup.com/bs_faq2.html#Hungarian
Andreas Stefik, Susanna Siebert, Melissa Stefik, and Kim Slattery. An empirical comparison of the accuracy rates of novices using the quorum, perl, and randomo programming languages. In *Proceedings of the 3rd ACM SIGPLAN Workshop on Evaluation and Usability of Programming Languages and Tools - PLATEAU '11*, 3. Portland, Oregon, USA, 2011. ACM Press. URL: <http://dl.acm.org/citation.cfm?doid=2089155.2089159> (visited on 2022-06-18), [doi:10.1145/2089155.2089159](https://doi.org/10.1145/2089155.2089159).
https://en.wikipedia.org/wiki/Syntactic_sugar

https://github.com/Mathnerd314/atomo/blob/master/prelude/core.atomo
https://craftofcoding.wordpress.com/2021/02/19/evolution-of-the-assignment-operator/
http://www.cse.iitm.ac.in/~amannoug/imop/tr-3.pdf
https://docs.python.org/3/reference/simple_stmts.html#assignment-statements
https://soc.me/languages/type-annotations
https://eblong.com/zarf/rule-language.html
https://lwn.net/Articles/779997/
https://github.com/ziglang/zig/issues/2165#issuecomment-478813464
https://www.gavilan.edu/csis/languages/comments.html
https://groups.google.com/g/golang-nuts/c/iHGLTFalb54/m/zqMoq9JRBAAJ
https://www.yarchive.net/comp/linux/coding_style.html
https://www.kernel.org/doc/html/latest/process/coding-style.html
https://github.com/jrevels/YASGuide#linealignmentspacing-guidelines
https://hoffa.medium.com/400-000-github-repositories-1-billion-files-14-terabytes-of-code-spaces-or-tabs-7cfe0b5dd7fd#.o7n8zeezx
https://stackoverflow.blog/2017/06/15/developers-use-spaces-make-money-use-tabs/
Richard J. Miara, Joyce A. Musselman, Juan A. Navarro, and Ben Shneiderman. Program indentation and comprehensibility. *Communications of the ACM*, 26(11):861--867, November 1983. URL: <https://dl.acm.org/doi/10.1145/182.358437> (visited on 2023-03-29), [doi:10.1145/182.358437](https://doi.org/10.1145/182.358437).
https://opensource.com/article/18/9/spaces-poll
http://www.opimedia.be/DS/languages/tabs-vs-spaces/
https://twitter.com/clattner_llvm/status/715572957720870912
https://www.audero.it/blog/2015/10/21/the-revolution-of-3-spaces-code-indentation/
https://www.reddit.com/r/ProgrammingLanguages/comments/uo0nq7/end_keywords_vs_pythonstyle_blocks_for_beginners/
https://en.wikipedia.org/wiki/Indentation_style
https://github.com/google/rune/blob/main/doc/rune4python.md
https://www.youtube.com/watch?v=94swlF55tVc
https://stackoverflow.com/a/453758
https://news.ycombinator.com/item?id=11419478
https://unspecified.wordpress.com/2011/10/18/why-pythons-whitespace-rule-is-right/
https://wiki.python.org/moin/Why%20separate%20sections%20by%20indentation%20instead%20of%20by%20brackets%20or%20%27end%27
https://stackoverflow.com/a/2149878
https://www.youtube.com/watch?v=SUIUZ09mnwM
https://en.wikipedia.org/wiki/Off-side_rule
https://stackoverflow.com/a/148880
Amanda Stent, Ann Syrdal, and Taniya Mishra. On the intelligibility of fast synthesized speech for individuals with early-onset blindness. In *The Proceedings of the 13th International ACM SIGACCESS Conference on Computers and Accessibility - ASSETS '11*, 211. Dundee, Scotland, UK, 2011. ACM Press. URL: <http://dl.acm.org/citation.cfm?doid=2049536.2049574> (visited on 2022-11-30), [doi:10.1145/2049536.2049574](https://doi.org/10.1145/2049536.2049574).
http://tvraman.github.io/emacspeak/manual/emacspeak_002dpython.html
https://groups.google.com/g/comp.lang.python/c/Dm-qTzO8Db8?hl=en#3216b7a02047873a
https://soc.me/languages/stop-using-angle-brackets-for-generics
https://docs.python.org/3/reference/expressions.html#expression-lists
Jeremy Singer and Blair Archibald. Functional baby talk: analysis of code fragments from novice Haskell programmers. *Electronic Proceedings in Theoretical Computer Science*, 270:37--51, May 2018. URL: <http://arxiv.org/abs/1805.05126v1> (visited on 2022-05-23), [doi:10.4204/EPTCS.270.3](https://doi.org/10.4204/EPTCS.270.3).
https://soc.me/languages/unified-condition-expressions
https://github.com/soc/soc.me/tree/main/_languages
The {{Ultimate Conditional Syntax}}, {Parreaux, Lionel}
https://en.wikipedia.org/wiki/Dangling_else
https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/MaybeNot.md
https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/ValueOfValuesLong.md
https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/SimpleMadeEasy-mostly-text.md
https://gemma.msl.ubc.ca/resources/baseCode/apidocs/ubic/basecode/util/StringUtil.html#append-java.lang.String-java.lang.String-java.lang.String-
https://jump.dev/JuMP.jl/dev/developers/style/#Juxtaposed-multiplication
https://ericlippert.com/2020/02/27/hundred-year-mistakes/
Gannon, J. D.: "Language Design to Enhance Programming Reliability," Report CSRG-47,Computer Systems Research Group, University of Toronto, Toronto, Canada, January 1975.
https://www.youtube.com/watch?v=rICThUCtJ0k
Victor Pankratius, Felix Schmidt, and Gilda Garreton. Combining functional and imperative programming for multicore software: An empirical study evaluating Scala and Java. In *2012 34th International Conference on Software Engineering (ICSE)*, 123--133. Zurich, June 2012. IEEE. URL: <https://web.archive.org/web/20120704064618id_/http://www.rz.uni-karlsruhe.de/~kb95/papers/pankratius-Scala-ICSE12-preprint.pdf> (visited on 2022-11-10), [doi:10.1109/ICSE.2012.6227200](https://doi.org/10.1109/ICSE.2012.6227200).
http://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.pdf
http://www.bramvandersanden.com/publication/pdf/sanden2014thesis.pdf
https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
https://github.com/google/rune/blob/main/doc/rune4python.md#looping
https://www.ccs.neu.edu/home/shivers/papers/loop.pdf
https://www.youtube.com/watch?v=PCzNwWmQdb0
https://github.com/core-lang/core/issues/44
https://blog.rust-lang.org/2017/03/02/lang-ergonomics.html#implicit-vs-explicit
https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler
https://en.wikipedia.org/wiki/Zig_(programming_language)
https://en.wikipedia.org/wiki/Rust_(programming_language)
https://coding-blog.vercel.app/posts/first-week
https://www.haskell.org/ghc/docs/3.02/users_guide/users_guide-1.html
https://rustc-dev-guide.rust-lang.org/about-this-guide.html
https://ziglang.org/learn/overview/
https://blog.reverberate.org/2009/07/giving-up-on-at-style-assembler-syntax.html
https://outerproduct.net/2021-02-13_att-asm.html
https://en.wikipedia.org/wiki/Fifth_Generation_Computer_Systems
https://en.wikipedia.org/wiki/RoboWar
https://www.youtube.com/watch?v=xrIjfIjssLE
https://github.com/matthiasn/talk-transcripts/tree/master/Hickey_Rich
https://austral-lang.org/spec/spec.html
https://discourse.elm-lang.org/t/status-update-3-nov-2021/7870
https://github.com/cheery/lever/
https://github.com/BSVino/JaiPrimer/blob/4a2d14f3e1c8e82a4ba68b81d3fd7d8d438e955c/JaiPrimer.md
https://pinafore.info/
http://github.com/codedot/lambda
https://github.com/manuel/wat-js
https://github.com/vito/atomo
https://github.com/vito/atomy
https://smartbear.com/blog/7-reasons-that-rexx-still-matters/
https://www.mathworks.com/help/matlab/math/multidimensional-arrays.html
https://www.jetbrains.com/lp/devecosystem-2021/ruby/#Ruby_what-web-development-tools-and-or-frameworks-do-you-regularly-use-if-any
https://android.googlesource.com/platform/system/vold/+/android-7.1.1_r11/AutoCloseFD.h
https://talks.golang.org/2012/splash.article
https://borretti.me/article/why-checked-exceptions-failed
https://forums.swift.org/t/se-0413-typed-throws/68507/
https://web.archive.org/web/20200510033212/https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/
https://kotlinlang.org/docs/exceptions.html#checked-exceptions
Programming Language Ideas That Work and Don't Work, 2022, Code Europe, Walter Bright https://www.youtube.com/watch?v=y7KWGv_t-MU
https://github.com/google/rune/blob/main/g3doc/index.md
https://2ality.com/2011/06/with-statement.html
https://wiki.c2.com/?NobodyAgreesOnWhatOoIs
https://wiki.c2.com/?DefinitionsForOo
https://discourse.julialang.org/t/workaround-for-traditional-inheritance-features-in-object-oriented-languages/1195/27
https://www.iso.org/obp/ui/#iso:std:iso-iec:2382:ed-1:v2:en
A. H. Borning. Classes versus prototypes in object-oriented languages. In *Proceedings of 1986 ACM Fall Joint Computer Conference*, ACM '86, 36--40. Washington, DC, USA, November 1986. IEEE Computer Society Press.
Bjarne Stroustrup. Why C++ is not just an object-oriented programming language. In *Addendum to the Proceedings of the 10th Annual Conference on Object-oriented Programming Systems, Languages, and Applications*, OOPSLA '95, 1--13. New York, NY, USA, October 1995. Association for Computing Machinery. URL: <https://www.stroustrup.com/oopsla.pdf> (visited on 2023-07-22), [doi:10.1145/260094.260207](https://doi.org/10.1145/260094.260207).
https://youtu.be/xcpSLRpOMJM?t=37
https://wiki.c2.com/?PolymorphismEncapsulationInheritance
http://paulgraham.com/reesoo.html
Ole Lehrmann Madsen, Birger Moller-Pedersen, and Kristen Nygaard. *Object-Oriented Programming in the Beta Programming Language*. Assn for Computing Machinery, Wokingham, England ; Reading, Mass, 2nd edition edition, January 1993. ISBN 978-0-201-62430-4. URL: <https://beta.cs.au.dk/Books/betabook.pdf>.
Bent Bruun Kristensen, Ole Lehrmann Madsen, and Birger Møller-Pedersen. The when, why and why not of the BETA programming language. In *Proceedings of the Third ACM SIGPLAN Conference on History of Programming Languages*, HOPL III, 10--1--10--57. New York, NY, USA, June 2007. Association for Computing Machinery. URL: <https://beta.alexandra.dk/sites/default/files/pdf/BETA-HOPL-V4.7_ref.pdf> (visited on 2023-08-04), [doi:10.1145/1238844.1238854](https://doi.org/10.1145/1238844.1238854).
Ole Lehrmann Madsen and Birger Møller-Pedersen. What object-oriented programming may be - and what it does not have to be. In Stein Gjessing and Kristen Nygaard, editors, *ECOOP '88 European Conference on Object-Oriented Programming*, Lecture Notes in Computer Science, 1--20. Berlin, Heidelberg, 1988. Springer. URL: <https://tidsskrift.dk/daimipb/article/view/7627>, [doi:10.1007/3-540-45910-3_1](https://doi.org/10.1007/3-540-45910-3_1).
https://archive.org/details/bitsavers_xeroxsmallructionManualMar76_5750953/page/n11/mode/2up smalltalk-72 instruction manual
http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-October/017019.html
https://en.wikipedia.org/w/index.php?diff=29999
https://crystal-lang.org/reference/1.9/syntax_and_semantics/everything_is_an_object.html
https://wiki.c2.com/?ObjectsAreDictionaries
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Working_with_Objects
{On Understanding Data Abstraction, Revisited}, {Cook, William R}, {2009},
https://www.youtube.com/watch?v=8uE6-vIi1rQ
Simon Marlow, Simon Peyton Jones, Edward Kmett, and Andrey Mokhov. Desugaring Haskell's do-notation into applicative operations. In *Proceedings of the 9th International Symposium on Haskell*, Haskell 2016, 92--104. New York, NY, USA, September 2016. Association for Computing Machinery. URL: <https://doi.org/10.1145/2976002.2976007> (visited on 2021-07-16), [doi:10.1145/2976002.2976007](https://doi.org/10.1145/2976002.2976007).
Levent Erkok. *Value Recursion in Monadic Computations*. PhD thesis, Oregon Health and Science University, October 2002. URL: <http://leventerkok.github.io/papers/erkok-thesis.pdf>.
https://ro-che.info/articles/2015-09-02-monadfix
https://gitlab.haskell.org/ghc/ghc/-/issues/5421
https://gitlab.haskell.org/ghc/ghc/-/issues/15349
https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Unsafe.html#v:unsafeFixIO
https://wiki.haskell.org/Evaluation_order_and_state_tokens
https://smallbone.se/no-state-monad.html
https://hackage.haskell.org/package/base-4.18.0.0/docs/src/Control.Monad.Fix.html#line-140
https://github.com/ekmett/kan-extensions/issues/64
https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Applicative.html#t:Applicative
http://docs.idris-lang.org/en/latest/tutorial/interfaces.html#notation
https://github.com/idris-lang/Idris-dev/issues/4395
https://hackage.haskell.org/package/kan-extensions-0.5.0/docs/Control-Monad-Codensity.html
http://blog.sigfpe.com/2008/12/mother-of-all-monads.html
http://blog.sigfpe.com/2008/12/mother-of-all-monads.html#c3279179532869319461
https://github.com/Mathnerd314/stroscot/blob/master/tests/Continuations-State.hs
https://hackage.haskell.org/package/transformers
Answer-Type Modification without Tears: Prompt-Passing Style Translation for Typed Delimited-Control Operators Ikuo Kobori, Yukiyoshi Kameyama, Oleg Kiselyov 2016
https://www.reddit.com/r/haskell/comments/6vu2i4/fun_exploration_right_kan_extensions_swapped/
https://hackage.haskell.org/package/kan-extensions-5.2.5/docs/Data-Functor-Kan-Ran.html
Philip Wadler. The essence of functional programming. In *Proceedings of the 19th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages - POPL '92*, 1--14. Albuquerque, New Mexico, United States, 1992. ACM Press. URL: <http://portal.acm.org/citation.cfm?doid=143165.143169> (visited on 2021-11-22), [doi:10.1145/143165.143169](https://doi.org/10.1145/143165.143169).
Leo A. Meyerovich and Ariel S. Rabkin. Socio-PLT: principles for programming language adoption. In *Proceedings of the ACM International Symposium on New Ideas, New Paradigms, and Reflections on Programming and Software - Onward! '12*, 39. Tucson, Arizona, USA, 2012. ACM Press. URL: <http://dl.acm.org/citation.cfm?doid=2384592.2384597> (visited on 2020-06-15), [doi:10.1145/2384592.2384597](https://doi.org/10.1145/2384592.2384597).
https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/
R.D. Bergeron, J.D. Gannon, D.P. Shecter, F.W. Tompa, and A. Van Dam. Systems Programming Languages. In *Advances in Computers*, volume 12, pages 175--284. Elsevier, 1972. URL: <https://linkinghub.elsevier.com/retrieve/pii/S0065245808605100> (visited on 2023-12-23), [doi:10.1016/S0065-2458(08)60510-0](https://doi.org/10.1016/S0065-2458(08)60510-0).
https://www.cs.kent.ac.uk/people/staff/dat/miranda/manual/30.html
https://rustc-dev-guide.rust-lang.org/overview.html
https://www.youtube.com/watch?v=Ju7v6vgfEt8
Wearing the Hair Shirt: A Retrospective on {{Haskell}} (2003)  Jones, Simon Peyton https://www.microsoft.com/en-us/research/publication/wearing-hair-shirt-retrospective-haskell-2003/
Rationale for the Design of the {{ADA}} Programming Language, Ichbiah, Jean D. and Heliard, Jean-Claude and Roubine, Olivier and Barnes, John G.P. and {Krieg-Brueckner}, Bernd and Wichmann, Brian A. https://apps.dtic.mil/sti/citations/ADA073854
https://eblong.com/zarf/essays/rule-based-if/ruleday-log.html
https://web.archive.org/web/20201017213150/https://devblogs.microsoft.com/oldnewthing/?p=36783
http://www.javapractices.com/topic/TopicAction.do?Id=25
https://stackoverflow.com/questions/2699209/java-io-ugly-try-finally-block
https://stackoverflow.com/questions/12552863/correct-idiom-for-managing-multiple-chained-resources-in-try-with-resources-bloc
https://vimeo.com/97329153
http://ddili.org/ders/d.en/scope.html
http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4152.pdf
https://hackage.haskell.org/package/resourcet
https://www.jot.fm/issues/issue_2022_02/article2.pdf
https://github.com/xtclang/xvm/discussions/40
https://en.wikipedia.org/wiki/Interval_(mathematics)#Including_or_excluding_endpoints
https://docs.julialang.org/en/v1/devdocs/types/
https://perl.plover.com/yak/12views/samples/slide045.html
https://shipilev.net/blog/2016/close-encounters-of-jmm-kind/#_horror_circus_kinda_works_but_horrifying
An analysis of programming language statement frequency in C, C++, and Java source code
https://www.cs.utexas.edu/~EWD/transcriptions/EWD05xx/EWD514.html
https://www.technologyreview.com/2002/07/01/40875/why-software-is-so-bad/
