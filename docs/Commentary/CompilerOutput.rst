Compiler output
###############

Output
======

The simplest compiler writes out a file like:

::

  -- <project name> <version> generated at <time/date>
  interpret = <boilerplate code for interpreter>
  data = "<contents of source file>"
  main = interpret data

Except in the proper object format.

Then the rest is optimizing/specializing this to run more efficiently.

Formats
=======

* static binary
* position-independent executable
* shared object / DLL  (``.so``, ``.dll``)
* static object file (``.o``). Stroscot uses incremental compilation rather than separate compilation, so static objects are considered an output format and are not produced when compiling to other formats.
* debug info (``.dwp``). Separate debug files reduces binary size, and Visual Studio on Windows only supports separate PDB files.

Executables have a single entry point, namely being executed. DLLs define a stable ABI / set of entry points. Inlining changes the ABI of the code, so we cannot do inlining or most other optimizations across the ABI boundary.

Symbols
=======

Jump/branch instructions take an address as a parameter. These addresses can be specified as an absolute memory location or relative to the program counter. Both of these require knowing the memory layout of the program. However, the addresses are stored in most object files as symbols and are not resolved until link time or load time. The actual value stored is a placeholder and it is fixed up by relocations.

When building a shared library the assembly is generated to minimize the number of relocations which must be applied, since they take time when starting the program. Position independent code will call non-static functions via the Procedure Linkage Table and reference global/static variables through Global Offset Tables. Local program counter-relative references do not need entries. The PLT and GOT tables are different for each process, but the actual code of the library is shared across all the processes. The indirection via the table `slowed down Python <https://bugs.python.org/issue38980>`__ by `27% <https://fedoraproject.org/wiki/Changes/PythonNoSemanticInterpositionSpeedup>`__ and is optimized away for non-extern functions in LLVM.

A symbol is a name and a value. In a C object file, there will be a symbol for each function and for each global and static variable, named similarly. These symbol's values will roughly be the address of the variable, i.e. the result of ``&my_global_var``.

Object files contain a table of references to all the symbols used by the code, as well as the locations in the code that the references are made, classified by type of reference (e.g. absolute vs relative references). The object file also contains a table of defined symbols, all the symbols which it exports. there can also be references to symbol names defined in a different object file, known as an undefined symbol.

Symbols also have versions, which are effectively part of the name of the symbol. But looking up an unversioned symbol resolves to the default versioned symbol.

Relocations
===========

During the linking process, the linker will assign an address to each defined symbol, and will resolve each undefined symbol by finding a defined symbol with the same name. Then it will perform relocations, modifications to the assembly code. A simple, and commonly used, relocation is “set this location in the contents to the value of this symbol plus this addend.” There are different kinds of relocations for different modes of addressing in the machine code.

The linker does not reorder sections of code, so relative jumps can be inserted fairly easily if you know the basic block sizes. The locations of absolute offsets need to be marked in the final executable so that the operating system loader can adjust them if it needs to load the executable somewhere other than its preferred address.

A relocation in an object file may refer to an undefined symbol. If the linker is unable to resolve that symbol, it will normally issue an error (but not always: for some symbol types or some relocation types an error may not be appropriate).

The linker also does some optimizations known as relaxation based on knowing final addresses. The most common type of relaxation is shrinking call instructions, e.g. replacing a 32-bit offset with a 16-bit offset. When the linker relaxes a relocation in the middle of the code, it may need to adjust any PC relative references which cross the point of the relaxation. Therefore, when relaxing, the assembler needs to generate relocation entries for all PC relative references. If the instruction size doesn't change these relocations are not required.

Linker
======

There are actually two linkers: the static linker, which creates a shared object or executable, and the dynamic loader, which finalizes addresses and performs relocations. The static linker mostly deals with sections while the dynamic loader uses segments; there are only a few types of segments but lots of section types.

It doesn't seem too ambitious to replace the static linker with a language-specific linker and object format, even though most languages e.g. GHC use the system assembler and linker. For example the Go project uses its own linker and object format (bastardized ELF). A custom format allows more freedom in defining symbols and for additional metadata to be stored in the objects, enhancing link time optimization. The end goal is completely getting rid of object files and storing the information in the same incremental build system database that the other phases of the compiler use.

The loader is much harder to replace. It is essentially part of the OS. The LoadLibrary call is built in on Windows, and hacking around this has `many limitations <https://www.codeproject.com/Tips/430684/Loading-Win-DLLs-manually-without-LoadLibrary>`__. On Linux the loader can be changed to a non-standard path, and libc can be forked, but distributing it would be difficult, and using a different shared object format would likely go the way of `FatELF <https://icculus.org/finger/icculus?date=2009-11-03&time=19-08-04>`__ (nowhere). Plus there are features like ASLR and lazy loading that would have to be reimplemented. So generally it seems we must live with it.

Linker algorithm
----------------

A linker needs to:

* Find all symbol definitions that live in each object file and library.
* Assign each symbol a final, absolute, address.
* Find all symbol references in each object file and library.
* Replace all symbol references with the absolute address of that symbol.
* Write completed executable to memory (loader) or file (linker).

Although the easiest algorithm is to deserialize all of the input objects in memory, this would mean the peak memory footprint includes the entirety of the inputs. The linker ends up eliminating a lot of symbols by discarding methods that don’t match any signatures in reachable types and cannot be called via reflection. The design should aim to keep data in cache.

Remove/cache work on the critical path (linker is critical). Use incremental build system with fingerprinting.

linker algorithm (based on `Go proposal <http://golang.org/s/better-linker>`__):
* goal: avoid deserializing relocs/metadata if possible
* (in parallel) mmap the inputs read-only

  * mmap manager to avoid mmap-ing too many files

* (mapreduce) build global symbol table

  *  while we can read the input symbol tables in any order, we must add them to the global symbol table in the order given on the command line.

* (in parallel) build bitmap of reachable symbol names by DFS through symbol table / inputs. Bias priority towards staying in package.
* don't compact reachable symbols, because keeping a simple mapping to the original symbol indexes is likely more valuable.
* mmap the output file read/write
* (in parallel) copy symbol data to output

  * store symbol data in temporary scratch with minimal lifetime

* apply relocations directly to the mmapped output.

new object format:
* symbol index
* fixed width
* pack byte data so mmap can skip over it
* int-indexed symbol table - global table built by the linker

  * native Go reference (import index, symbol index)

    * imported package table in referencing code unit
    * exported symbol definition table in each package

  * Linknamed symbols - symbols defined in assembly that can only be resolved via their names.

    * identified at their definition site, discard after object loading.

  * “Dupok” symbols - coalesced/deduplicated symbols. content-addressed and deduplicated via a separate table.

directly load a Go object file into its running image
link and execute a test in a single step, rather than producing a binary that will be discarded almost immediately

if all code is position-independent and we retain entire packages, then all regular symbol references can be done with nothing more than the base address of the package that contains the symbol, by statically baking in the offsets of all the symbols. This would make offset tables incredibly small, though would make cross-package symbol references more expensive.

processing relocations should be a fundamental part of any package for working with object files.

Debugging information
=====================

Debugging information is essentially a complete fiction. After optimization and transformation the output machine code bears no resemblance to the original program. But debuggers needs to know which machine code instruction corresponds to which source code location. So DWARF information should be generated as early as possible, ideally right after parsing, and then propagated through each transformation.

DWARF is oriented around traditional compilation units for procedural languages and thus it might not quite flexible enough for our purposes. But it's a standard and GHC managed to hack support in so it should be followed as closely as practical.

There is also `PDB <https://llvm.org/docs/PDB/index.html>`__, but it is sparsely documented and there are various DWARF tools for Windows while no PDB tools at all for Linux, so DWARF wins as a format. Sadly this means stack traces on Windows will not extend to system libraries, but those are generally treated as black boxes anyway.

Non-executable stack
====================

When the Linux kernel starts a program, it looks for a PT_GNU_STACK segment. If it does not find one, it sets the stack to be executable (if appropriate for the architecture). If it does find a PT_GNU_STACK segment, it marks the stack as executable if the segment flags call for it. (It’s possible to override this and force the kernel to never use an executable stack.) Similarly, the dynamic linker looks for a PT_GNU_STACK in any executable or shared library that it loads, and changes the stack to be executable if any of them require it. When this all works smoothly, most programs wind up with a non-executable stack. The most common reason this fails is that part of the program is written in assembler, and the assembler code does not create a .note.GNU_stack section. If you write assembler code for GNU/Linux, you must always be careful to add the appropriate line to your file. For most targets, the line you want is ``.section .note.GNU-stack,"",@progbits`` There are some linker options to control this. The -z execstack option tells the linker to mark the program as requiring an executable stack, regardless of the input files. The -z noexecstack option marks it as not requiring an executable stack. The gold linker has a --warn-execstack option which will cause the linker to warn about any object which is missing a .note.GNU-stack option or which has an executable .note.GNU-stack option. The execstack program may also be used to query whether a program requires an executable stack, and to change its setting.

ASLR
====

Modern ELF systems can randomize the address at which shared libraries are loaded. This is generally referred to as Address Space Layout Randomization, or ASLR. Shared libraries are always position independent, which means that they can be loaded at any address. Randomizing the load address makes it slightly harder for attackers of a running program to exploit buffer overflows or similar problems, because they have no fixed addresses that they can rely on. ASLR is part of defense in depth: it does not by itself prevent any attacks, but it makes it slightly more difficult for attackers to exploit certain kinds of programming errors in a useful way beyond simply crashing the program.

Although it is straightforward to randomize the load address of a shared library, an ELF executable is normally linked to run at a fixed address that can not be changed. This means that attackers have a set of fixed addresses they can rely on. Permitting the kernel to randomize the address of the executable itself is done by generating a Position Independent Executable, or PIE.

It turns out to be quite simple to create a PIE: a PIE is simply an executable shared library. To make a shared library executable you just need to give it a PT_INTERP segment and appropriate startup code. The startup code can be the same as the usual executable startup code, though of course it must be compiled to be position independent.

When compiling code to go into a shared library, you use the -fpic option. When compiling code to go into a PIE, you use the -fpie option. Since a PIE is just a shared library, these options are almost exactly the same. The only difference is that since -fpie implies that you are building the main executable, there is no need to support symbol interposition for defined symbols. In a shared library, if function f1 calls f2, and f2 is globally visible, the code has to consider the possibility that f2 will be interposed. Thus, the call must go through the PLT. In a PIE, f2 can not be interposed, so the call may be made directly, though of course still in a position independent manner. Similarly, if the processor can do PC-relative loads and stores, all global variables can be accessed directly rather than going through the GOT.

Other than that ability to avoid the PLT and GOT in some cases, a PIE is really just a shared library. The dynamic linker will ask the kernel to map it at a random address and will then relocate it as usual.

This does imply that a PIE must be dynamically linked, in the sense of using the dynamic linker. Since the dynamic linker and the C library are closely intertwined, linking the PIE statically with the C library is unlikely to work in general. It is possible to design a statically linked PIE, in which the program relocates itself at startup time. The dynamic linker itself does this. However, there is no general mechanism for this at present.

ELF
===

The kernel/loader only uses segments when loading executables into memory. So we don't need to bother with sections. Ignoring one-offs and notes (comments), there is only one segment `type <http://www.sco.com/developers/gabi/latest/ch5.pheader.html#p_type>`__, , a loadable segment PT_LOAD. The attributes are ``flags, offset, vaddr, filesz, memsz, align``. ``filesz <= memsz``. ``filesz`` bytes starting from offset from the file are mapped to memory starting at ``vaddr``. If ``memsz > filesz``, the extra bytes are defined to hold the value 0 and to follow the segment's initialized area. ``vaddr - offset mod align == 0``; ``align`` is usually the page size. ``flags`` defines the permissions that mmap uses and can be any combination of read/write/execute.

So ignoring the file format / alignment / special handling of ending with 0's, a loadable segments is ``Load {flags, vaddr, contents : [Byte]}``.

`PE <https://docs.microsoft.com/en-us/windows/win32/debug/pe-format>`__ is similar, the handling of alignment is different. Mach-O doesn't even have a man page currently available from Apple so who cares.

Assembly
========

A segment may have executable pieces but `also <https://stackoverflow.com/questions/55607052/why-do-compilers-put-data-inside-textcode-section-of-the-pe-and-elf-files-and>`__ pieces of non-executed data: this is used with GHC's `tables-next-to-code layout <https://lists.llvm.org/pipermail/llvm-dev/2012-February/047555.html>`__ and also ARM's "constant islands" or `literal pools <https://en.wikipedia.org/wiki/Literal_pool>`__. Conceptually the pieces are just smaller segments, but an actual segment is sized to a multiple of the page size. So to convert pieces to segments we would start with single-page segments with permissions the union of the permissions of the contained pieces, zeroing the memory if no piece defines it, and then merge together adjacent segments with the same permissions.

We can write executable pieces using our instruction templates, ``Piece = [{flags | executable, vaddr, contents : [Instruction]}]``, where ``Instruction = (InstructionTemplate, Operands)`` (or actually an ADT because the number of operands is fixed by the template).

Labels: we split ``[Instruction]`` in each executable piece into blocks, ``(Label,Block) where Block = [Instruction]``. To form a piece the labels are erased and the blocks concatenated.

determine the size of all the assembled code and data
generate code using symbol addresses
code's size cannot depend on the value of a symbol declared after the code in question.

Code layout: a little 1D `constraint language <https://developer.android.com/reference/androidx/constraintlayout/widget/ConstraintLayout>`__:
* fixed address
* start/end of A is eq/leq/geq/lt/gt a constant plus start/end of B
* align x A, ensure start of A is a multiple of x.

Generally the smaller layout wins, but more specifically the layout should be optimized for cache coherence - hot paths with hot, cold with cold.

Testing
=======

The main strategy is "golden tests". To start you run some part of the compiler and serialize the output to some human-readable format. Then you store that data as the "expected" output, and run the test again every compile to make sure it still produces the expected output, comparing with diff or similar. Then you check a few changed outputs to find any bugs, fix what needs fixing, and for any non-bug changes, there should be a way to automatically update all the expected outputs to the current outputs.

For a parser, the main test is "integration testing" by giving it examples of source code, full modules, and verifying that it parses to the right AST / fails with the expected output. There is also "unit testing" where you test each grammar production rule individually, e.g. parse an expression or a statement or a block.

Another parser test is "stress testing", generating random valid code samples. The generator doesn't need to exercise every parser path, just the common ones. It pays off in that you understand the grammar better and you can test the performance of your parser.

For the IR golden tests are fine (compare after each optimization pass).
