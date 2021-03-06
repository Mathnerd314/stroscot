Library
#######

Stroscot will support the standard libraries of other languages, so e.g. if you want the C functions with C semantics you would ``import Library.C``. Compatibility is a natural step to world domination.

Standard libraries:
* `Rust <https://github.com/rust-lang/rust/tree/master/library>`__ (MIT + Apache 2.0)
* `Go <https://github.com/golang/go/tree/master/src>`__ (BSD-style)
* `Haskell <https://gitlab.haskell.org/ghc/ghc/-/tree/master/libraries>`__ (BSD-style)

  * The alternate prelude `Foundation <https://github.com/haskell-foundation/foundation>`__ (BSD)

* Julia `1 <https://github.com/JuliaLang/julia/tree/master/base>`__ `2 <https://github.com/JuliaLang/julia/tree/master/stdlib>`__ (MIT)
* C

  * `glibc <https://sourceware.org/git/?p=glibc.git;a=tree>`__ (LGPLv2.1, some files BSD/ISC/etc.)
  * `Musl <https://git.musl-libc.org/cgit/musl/tree/>`__ (MIT)

* Python `1 <https://github.com/python/cpython/tree/master/Modules>`__ `2 <https://github.com/python/cpython/tree/master/Lib>`__ (PSFv2)
* `Zig <https://github.com/ziglang/zig/tree/master/lib/std>`__ (MIT)
* Slate `1 <https://github.com/briantrice/slate-language/tree/master/src/core>`__ `2 <https://github.com/briantrice/slate-language/tree/master/src/lib>`__ `3 <https://github.com/briantrice/slate-language/tree/master/src/i18n>`__

But building on the work of others isn't enough, we also have to improve and synthesize a new, universal standard library for new programs to use. For this, the proposals of the various languages are useful, as they encapsulate changes and include motivation as to why the change was made. A feature of a language might be historical accident but a proposal is always a deliberate design choice. Even the rejected proposals are useful as they indicate language/library "smells", areas that could use improvement.

* `GHC <https://github.com/ghc-proposals/ghc-proposals/pulls>`__
* `Python <https://github.com/python/peps>`__
* `Rust <https://github.com/rust-lang/rfcs/pulls>`__ (`accepted <https://rust-lang.github.io/rfcs/>`__)
* `Go <https://github.com/golang/go/labels/Proposal>`__

Numbers
=======

In the mathematical world there are integers and real numbers, which have well-defined arithmetic operations (besides division by 0). In the computer world we do not have either of these luxurious spaces, but rather various formats for numbers which represent subsets of the space.

Literals
--------

Literals are parsed into records like ``NumberLiteral { digits = "123", exponent = "24" }``. We can define implicit conversions to the various the numeric types. Leadings 0's restrict the type, so ``010`` must be stored in a type that can contain 999.

Integers
--------

.. raw:: html

  <div style="display: none">
  \[
  \newcommand{\seq}[1]{{\langle #1 \rangle}}
  \newcommand{\abs}[1]{{\vert #1 \rvert}}
  \newcommand{\sem}[1]{[\![ #1 ]\!]}
  \]
  </div>


The most common integer format is a signed/unsigned integer with the range :math:`[0,2^{k}-1]` or :math:`[-2^{k-1},2^{k-1}-1]`, taking :math:`k` bits. But it is not too tricky to implement efficient arithmetic operations for arbitrarily-ranged integers :math:`[a,b)`, where the modulus :math:`b-a` is a power of 2. We can represent as :math:`a+k` or :math:`b-k` where :math:`k` is unsigned or :math:`(a+b)/2 + k` for signed :math:`k`. The operations use :math:`\log_2 (b-a)` bits and expand the constants out (:math:`(x+a)+(y+a)=(x+y)+2*a`, etc. - there's definitely clever ways to structure the computations for efficiency). When the range can be determined statically there is no overhead besides the extra operations (and there are no extra operations if the range fits into the machine-sized integer). If we use branching operations we can go even farther and use a tag bit to represent unions of ranges, :math:`[-23,2] \cup [56,100]`.

With these extended ranges, the key difference between "signed" and "unsigned" is not that signed can represent negative numbers, but rather that signed integers represent an unbounded integer, that errors if the result is not representable (overflow, underflow, gap missing), while unsigned integers represent equivalence classes :math:`\sem{a} = \{ a + k m \mid k \in \mathbb{N} \}`, :math:`m` being the modulus. The format defines the representatives used, operations are done in :math:`\mathbb{Z}` on the representatives, and then the result is converted via the equivalence class to a representative. So better names might be signed integer format = erroring integer format, unsigned integer format = wrapping integer format.

Division for all of these formats is defined using the `division algorithm for Euclidean domains <https://en.wikipedia.org/wiki/Euclidean_domain>`__. For :math:`a, b \mid b \neq 0`, :math:`a divMod b` produces :math:`(q,r)` such that :math:`a = bq + r` and the norm :math:`\abs{r}` is minimized. This gives "round to nearest" behavior and is different from most other programming languages, e.g. ``11 divMod 4 = (3,-1)`` rather than ``(2,3)``. But mathematically it has nice properties. Ties are broken by choosing positive :math:`r`, this amounts to tweaking the norm function so :math:`\abs{+x} = x - 0.1`. We can also consider other variants like setting :math:`\abs{-x} = \infty`, this gives Euclidean division. For a complicated split-range number number format, the computation will probably have to use brute force to determine the result. The range of :math:`q` is another question, most likely we have to give it as an argument.

The behavior is different from `most other programming languages <https://en.wikipedia.org/wiki/Modulo_operation#In_programming_languages>`__. In particular the C / assembly behavior of truncation is just wrong and cannot be emulated with a norm function - there is no consistent ranking giving ``1 divmod 2 = (0, 1)``, ``-1 divmod 2 = (0, -1)``. But of course C's behavior can still be defined for the relevant formats.

Fractions
---------

The simplest is ratios :math:`a / b`, using integers over some domain. Fixed-point arithmetic is a special case of this where :math:`b` is fixed. Floating point numbers are an integer mantissa times an integer radix raised to an integer exponent. The radix is usually 2 but `IEEE-754 <https://en.wikipedia.org/wiki/IEEE_754>` has also defined decimal floating point (radix 10). The exponent itself is another integer, usually restricted to a quite small range. We can also include posits; these are mantissa * radix ^ exponent * useed ^ regime, where the first part is the floating point stuff, useed is 2 ^ 2 ^ maximum exponent size, and the regime is nonnegative.

Actual types
------------

We could try to define generic integer/float types, but only a few have efficient arithmetic operations. So in practice we have only ``sN`` / ``uN`` (for ``N`` restricted to 8/16/32/64), ``Float``, and ``Double``. Differently-ranged integers, fixed-point arithmetic, unums, and posits can all be defined in libraries. It would also be good to have arbitrary-precision types, like `GMP <https://gmplib.org/>`__'s integer/rational and `MFPR <https://www.mpfr.org/>`__'s float that uses an s32/s64 exponent and an arbitrary precision mantissa. The binding could be at the C level like `Haskell's integer-gmp <https://hackage.haskell.org/package/integer-gmp>`__ or it could use the assembly routines directly.

Operations
----------

For arithmetic we define implicit conversions, ``convert : s8 -> Arb`` and so on to an arbitrary precision type ``Arb`` with the usual arithmetic operations, ``(+) : Arb -> Arb -> Arb`` and so on. Then narrowing the result back into a restrictive format is represented explicitly with an operation, ``narrow s16 (2+30*x)`` and so on. The compiler then figures out how to compute the answer as efficiently as possible. For floating point the narrowing also takes a precision argument, or optimizes for the best precision like Herbie, depending on whether speed or accuracy is preferred.

For compatibility with other languages we can define narrowed arithmetic operations, like ``a + b = assert(a is s16 && b is s16); x = narrow s16 (a+b); assert(x is s16)``. These give an error if the result doesn't fit. We can also support implicit conversions ``convert : s8 -> s16`` and so on; the compiler has to check that the narrowed arbitrary-precision computation matches the various fixed-width computations, but it should be resolvable.

Floating points numbers don't have implicit conversions between each other, besides the conversion from literals. The arithmetic operations are defined normally, ``(+) :: f32 -> f32 -> f32`` and so on.

Strings
=======

The standard, terrible null-terminated C string will always be needed, but most purposes should be satisfied by using an array / buffer of bytes together with a length. There can be different encodings: UTF8, UTF16, UTF32, or some other encodings like Shift JIS or Big5. UTF8 is the most common so it should be the default.

Invalid characters can be handled different ways according to a mode parameter: delete from string, preserve, transcode to private use area, etc.

Non-mutating views are easy to implement as auxiliary data structures that share the underlying string. So we can have substrings / slices and codepoint/grapheme/word indexing.

For mutation we can't in general replace the contents in-place, because they're different lengths. So copying is the way to go. But a more advanced implementation would use ropes or similar.

I/O
===

The general API for I/O follows the io_uring design, we write a bunch of operations to a buffer and then execute callbacks based on the result.
We also need datatypes for dealing with streaming I/O, but continuations work for that.

The functions themselves are written in the token-passing style ``RealWorld, a -o RealWorld, b``, passing around the ``RealWorld`` token.

Errors
======

``{}.x`` produces an error. But how do errors behave? Since it is a value and we are dealing with value operations we get back a special kind of value, an error value like ``NoSuchAttributeError {} "x"``.

Similarly invalid pointer reads should return ``InvalidPointer``, rather than crashing the program. Pointer reads generate page faults, which if they are invalid will be returned to the program via the signal "Segmentation fault" (SIGSEGV). C/C++ `can't handle these easukt <https://stackoverflow.com/questions/2350489/how-to-catch-segmentation-fault-in-linux>`__ because they are `synchronous signals <https://lwn.net/Articles/414618/>`__ and signal behavior is mostly left undefined, but in fact signals are `fairly well-behaved <https://hackaday.com/2018/11/21/creating-black-holes-division-by-zero-in-practice/>`__ (`OpenSSL <https://sources.debian.org/src/openssl/1.1.1k-1/crypto/s390xcap.c/?hl=48#L48>`__'s method of recovering from faults even seems standards-compliant). It definitely seems possible to implement this as an error value in a new language. Go `allows <https://stackoverflow.com/questions/43212593/handling-sigsegv-with-recover>`__ turning (synchronous) signals into "panics" that can be caught with recover.

Division by zero should be handled in the same way, producing ``DivisionByZeroError``. On AMD64 DIV by 0 produces a fault, which on Linux the kernel picks up and sends to the application as a SIGFPE. In contrast, UDIV by 0 on ARM64 simply produces 0. So on ARM64 handling division by 0 requires checking if the argument is zero beforehand. But on AMD64 we can check the value or implement it via signals; it'll require testing to see which is faster in typical programs.



Back in March Go picked up a dynamic exception mechanism. Previously, when code called the panic function, it simply aborted your program. Now, it walks up the stack to the top of the currently running goroutine, running all deferred functions. More interestingly, if a deferred function calls the new recover function, the panic is interrupted, and stops walking the stack at that point. Execution then continues normally from the point where recover was called. The recover function returns the argument passed to panic, or nil if there is no panic in effect.

I just completed the implementation of this in gccgo. It turned out to be fairly complex, so I’m writing some notes here on how it works.

The language requires that panic runs the deferred functions before unwinding the stack. This means that if the deferred function calls runtime.Callers (which doesn’t work in gccgo, but never mind, it will eventually) it gets a full backtrace of where the call to panic occurred. If the language did not work that way, it would be difficult to use recover as a general error handling mechanism, because there would be no good way to dump a stack trace. Building up a stack trace through each deferred function call would be inefficient.

The language also requires that recover only return a value when it is called directly from a function run by a defer statement. Otherwise it would be difficult for a deferred function to call a function which uses panic and recover for error handling; the recover might pick up the panic for its caller, which would be confusing.

As a general gccgo principle I wanted to avoid requiring new gcc backend features. That raised some difficulty in implementing these Go language requirements. How can the recover function know whether it is being invoked directly by a function started by defer? In 6g, walking up the stack is efficient. The panic function can record its stack position, and the recover function can verify that it is at the correct distance below. In gccgo, there is no mechanism for reliably walking up the stack other than exception stack unwinding, which does not provide a helpful API. Even if it did, gccgo’s split stack code can introduce random additional stack frames which are painful to account for. And there is no good way for panic to mark the stack in gccgo.

What I did instead was have the defer statement check whether the function is it deferring might call recover (e.g., it definitely calls recover, or it is a function pointer so we don’t know). In that case, the defer statement arranges to have the deferred thunk record the return address of the deferred function at the top of the defer stack. This value is obtained via gcc’s address-of-label extension, so no new feature was required. This gives us a value which a function which calls recover can check, because a function can always reliably determine its own return address via gcc’s __builtin_return_address function.

However, if the stack is split, then __builtin_return_address will return the address of the stack splitting cleanup code rather than the real caller. To avoid that problem, a function which calls recover is split into two parts. The first part is a small thunk which is marked to not permit its stack to be split. This thunk gets its return address and checks whether it is being invoked directly from defer. It passes this as a new boolean parameter to the real function, which does permit a split stack. The real function checks the new parameter before calling recover; if it is false, it just produces a nil rather than calling recover. The real function is marked uninlinable, to ensure that it is not inlined into its only call site, which could blow out the stack.

That is sufficient to let us know whether recover should return a panic value if there is one, at the cost of having an extra thunk for every function which calls recover. Now we can look at the panic function. It walks up the defer stack, calling functions as it goes. When a function sucessfully calls recover, the panic stack is marked. This stops the calls to the deferred functions, and starts a stack unwind phase. The stack unwinding is done exactly the way that g++ handles exceptions. The g++ exception mechanism is general and cross-language, so this part was relatively easy. This means that every function that calls recover has an exception handler. The exception handlers are all the same: if this is the function in which recover returned a value, then simply return from the current function, effectively stopping the stack unwind. If this is not the function in which recover returned a value, then resume the stack unwinding, just as though the exception were rethrown in C++.

This system is somewhat baroque but it appears to be working. Everything is reasonably efficient except for a call to recover which does not return nil; that is as expensive as a C++ exception. Perhaps I will think of ways to simplify it over time.



Throwing an exception in C++ requires more than unwinding the stack. As the program unwinds, local variable destructors must be executed. Catch clauses must be examined to see if they should catch the exception. Exception specifications must be checked to see if the exception should be redirected to the unexpected handler. Similar issues arise in Go, Java, and even C when using gcc’s cleanup function attribute.

As I described earlier, each CIE in the unwind data may contain a pointer to a personality function, and each FDE may contain a pointer to the LSDA, the Language Specific Data Area. Each language has its own personality function. The LSDA is only used by the personality function, so it could in principle differ for each language. However, at least for gcc, every language uses the same format, since the LSDA is generated by the language-independent middle-end.

The personality function takes five arguments:

    A int version number, currently 1.
    A bitmask of actions.
    An exception class, a 64-bit unsigned integer which is specific to a language.
    A pointer to information about the specific exception being thrown.
    Unwinder state information.

The exception class permits code written in one language to work correctly when an exception is thrown by code written in a different language. The value for g++ is
“GNUCC++\0” (or “GNUCC++\1” for a dependent exception, which is used when rethrowing an exception). The value for Go is “GNUCGO\0\0”. The exception specific information can only be examined if the exception class is recognized.

Unwinding the stack for an exception is done in two phases. In the first phase, the unwinder walks up the stack passing the action _UA_SEARCH_PHASE (which has the value 1) to each personality function that it finds. The personality function should examine the LSDA to see if there is a handler for the exception being thrown. It should return _URC_HANDLER_FOUND (6) if there is or _URC_CONTINUE_UNWIND (8) if there isn’t. The search phase will continue until a handler is found or until the top of the stack is reached. The unwinder will not actually change anything while walking. If the top of the stack is reached the unwinder will simply return, and the calling code will take the appropriate action, which for C++ is to call std::terminate. Because of the two phase unwinding approach, if std::terminate dumps core, a backtrace will show the code which threw the exception.

If a handler is found, the second phase begins. The unwinder walks up the stack passing the action _UA_CLEANUP_PHASE (2) to each personality function. The unwinder will also set _UA_FORCE_UNWIND (8) in the actions bitmask if the personality function may not catch the exception, because the unwinding is happening due to some event like thread cancellation. The unwinder will walk up the stack until it finds the handler—the stack frame for which the personality function returned _URC_HANDLER_FOUND. When it calls that function, the unwinder will pass _UA_HANDLER_FRAME (4) in the actions bitmask. This time, the unwinder will changes things as it goes, removing stack frames.

In order to run destructors, the personality function will call _Unwind_SetIP on the context parameter to set the program counter to point to the cleanup routine, and then return _URC_INSTALL_CONTEXT (7) to tell the unwinder to branch to the current context. The address which starts the cleanup is known as a landing pad. The cleanup should do whatever it needs to do, and then call _Unwind_Resume. The exception information needs to be passed to _Unwind_Resume. The personality routine arranges to pass the exception information to the cleanup by calling _Unwind_SetGR passing __builtin_eh_return_data_regno(0) and the exception information passed to the personality routine. Each target which supports this approach has to dedicate two registers to holding exception information. This is the first one.

The personality function which finds the handler works pretty much the same way. It may also use _Unwind_SetGR to set a value in __builtin_eh_return_data_regno(1) to indicate which exception was found. The exception handler may rethrow the exception via _Unwind_RaiseException or it may simply continue a normal execution path.

At this point we’ve seen everything except how the personality function decides whether it needs to run a cleanup or catch an exception. The personality function makes this decision based on the LSDA. As mentioned above, while the LSDA could be language dependent, in practice it is not. There is a different personality function for each language, but they all do more or less the same thing, omitting aspects which are not relevant for the language (e.g., there is a personality function for C, but it only runs cleanups and does not bother to look for exception handlers).

The LSDA is found in the section .gcc_except_table (the personality function is just a function and lives in the .text section as usual). The personality function gets a pointer to it by calling _Unwind_GetLanguageSpecificData. The LSDA starts with the following fields:

    A 1 byte encoding of the following field (a DW_EH_PE_xxx value).
    If the encoding is not DW_EH_PE_omit, the landing pad base. This is the base from which landing pad offsets are computed. If this is omitted, the base comes from calling _Unwind_GetRegionStart, which returns the beginning of the code described by the current FDE. In practice this field is normally omitted.
    A 1 byte encoding of the entries in the type table (a DW_EH_PE_xxx value).
    If the encoding is not DW_EH_PE_omit, the types table pointer. This is an unsigned LEB128 value, and is the byte offset from this field to the start of the types table used for exception matching.
    A 1 byte encoding of the fields in the call-site table (a DW_EH_PE_xxx value).
    An unsigned LEB128 value holding the length in bytes of the call-site table.

This header is immediately followed by the call-site table. Each entry in the call-site table has four fields. The number of bytes in the header gives the total length. Each entry in the call-site table describes a particular sequence of instructions within the function that the FDE desribes.

    The start of the instructions for the current call site, a byte offset from the landing pad base. This is encoded using the encoding from the header.
    The length of the instructions for the current call site, in bytes. This is encoded using the encoding from the header.
    A pointer to the landing pad for this sequence of instructions, or 0 if there isn’t one. This is a byte offset from the landing pad base. This is encoded using the encoding from the header.
    The action to take, an unsigned LEB128. This is 1 plus a byte offset into the action table. The value zero means that there is no action.

The call-site table is sorted by the start address field. If the personality function finds that there is no entry for the current PC in the call-site table, then there is no exception information. This should not happen in normal operation, and in C++ will lead to a call to std::terminate. If there is an entry in the call-site table, but the landing pad is zero, then there is nothing to do: there are no destructors to run or exceptions to catch. This is a normal case, and the unwinder will simply continue. If the action record is zero, then there are destructors to run but no exceptions to catch. The personality function will arrange to run the destructors as described above, and unwinding will continue.

Otherwise, we have an offset into the action table. Each entry in the action table is a pair of signed LEB128 values. The first number is a type filter. The second number is a byte offset to the next entry in the action table. A byte offset of 0 ends the current set of actions.

A type filter of zero indicates a cleanup, which is the same as an action record of zero in the call-site table. This means that there is a cleanup to be called even if none of the types match.

A positive type filter is an index into the types table. This is a negative index: the value 1 means the entry preceding the types table base, 2 means the entry before that, etc. The size of entries in the types table comes from the encoding in the header, as does the base of the types table. Each entry in the types table is a pointer to a type information structure. If this type information structure matches the type of the exception, then we have found a handler for this exception. The type filter value is a switch value will be passed to the handler in exception register 1. The actual comparison of the type information, and determining the type information from the exception pointer, really is language dependent. In C++ this is a pointer to a std::type_info structure. A NULL pointer in the types table is a catch-all handler.

A negative type filter is a byte offset into the types table of a NULL terminated list of pointers to type information structures. If the type of the current exception does not match any of the entries in the list, then there is an exception specification error. This is treated as an exception handler with a negative switch value.

I think that covers everything about how gcc unwinds the stack and throws exceptions.


If you followed my last post, you will see that in order to unwind the stack you have to find the FDE associated with a given program counter value. There are two steps to this problem. The first one is finding the CIEs and FDEs at all. The second one is, given the set of FDEs, finding the one you need.

The old way this worked was that gcc would create a global constructor which called the function __register_frame_info, passing a pointer to the .eh_frame data and a pointer to the object. The latter pointer would indicate the shared library, and was used to deregister the information after a dlclose. When looking for an FDE, the unwinder would walk through the registered frames, and sort them. Then it would use the sorted list to find the desired FDE.

The old way still works, but these days, at least on GNU/Linux, the sorting is done at link time, which is better than doing it at runtime. Both gold and the GNU linker support an option --eh-frame-hdr which tell them to construct a header for all the .eh_frame sections. This header is placed in a section named .eh_frame_hdr and also in a PT_GNU_EH_FRAME segment. At runtime the unwinder can find all the PT_GNU_EH_FRAME segments by calling dl_iterate_phdr.

The format of the .eh_frame_hdr section is as follows:

    A 1 byte version number, currently 1.
    A 1 byte encoding of the pointer to the exception frames. This is a DW_EH_PE_xxx value. It is normally DW_EH_PE_pcrel | DW_EH_PE_sdata4, meaning a 4 byte relative offset.
    A 1 byte encoding of the count of the number of FDEs in the lookup table. This is a DW_EH_PE_xxx value. It is normally DW_EH_PE_udata4, meaning a 4 byte unsigned count.
    A 1 byte encoding of the entries in the lookup table. This is a DW_EH_PE_xxx value. It is normally DW_EH_PE_datarel | DW_EH_PE_sdata4, meaning a 4 byte offset from the start of the .eh_frame_hdr section. That is the only encoding that gcc’s current unwind library supports.
    A pointer to the contents of the .eh_frame section, encoded as indicated by the second byte in the header. This pointer is only used if the format of the lookup table is not supported or is for some reason omitted..
    The number of FDE pointers in the table, encoded as indicated by the third byte in the header. If there are no FDEs, the encoding can be DW_EH_PE_omit and this number will not be present.
    The lookup table itself, starting at a 4-byte aligned address in memory. Assuming the fourth byte in the header is DW_EH_PE_datarel | DW_EH_PE_sdata4, each entry in the table is 8 bytes long. The first four bytes are an offset to the initial PC value for the FDE. The last four byte are an offset to the FDE data itself. The table is sorted by starting PC.

Since FDEs do not overlap, this table is sufficient for the stack unwinder to quickly find the relevant FDE if there is one.


When gcc generates code that handles exceptions, it produces tables that describe how to unwind the stack. These tables are found in the .eh_frame section. The format of the .eh_frame section is very similar to the format of a DWARF .debug_frame section. Unfortunately, it is not precisely identical. I don’t know of any documentation which describes this format. The following should be read in conjunction with the relevant section of the DWARF standard, available from http://dwarfstd.org.

The .eh_frame section is a sequence of records. Each record is either a CIE (Common Information Entry) or an FDE (Frame Description Entry). In general there is one CIE per object file, and each CIE is associated with a list of FDEs. Each FDE is typically associated with a single function. The CIE and the FDE together describe how to unwind to the caller if the current instruction pointer is in the range covered by the FDE.

There should be exactly one FDE covering each instruction which may be being executed when an exception occurs. By default an exception can only occur during a function call or a throw. When using the -fnon-call-exceptions gcc option, an exception can also occur on most memory references and floating point operations. When using -fasynchronous-unwind-tables, the FDE will cover every instruction, to permit unwinding from a signal handler.

The general format of a CIE or FDE starts as follows:

    Length of record. Read 4 bytes. If they are not 0xffffffff, they are the length of the CIE or FDE record. Otherwise the next 64 bits holds the length, and this is a 64-bit DWARF format. This is like .debug_frame.
    A 4 byte ID. For a CIE this is 0. For an FDE it is the byte offset from this field to the start of the CIE with which this FDE is associated. The byte offset goes to the length record of the CIE. A positive value goes backward; that is, you have to subtract the value of the ID field from the current byte position to get the CIE position. This differs from .debug_frame in that the offset is relative rather than being an offset into the .debug_frame section.

A CIE record continues as follows:

    1 byte CIE version. As of this writing this should be 1 or 3.
    NUL terminated augmentation string. This is a sequence of characters. Very old versions of gcc used the string “eh” here, but I won’t document that. This is described further below.
    Code alignment factor, an unsigned LEB128 (LEB128 is a DWARF encoding for numbers which I won’t describe here). This should always be 1 for .eh_frame.
    Data alignment factor, a signed LEB128. This is a constant factored out of offset instructions, as in .debug_frame.
    The return address register. In CIE version 1 this is a single byte; in CIE version 3 this is an unsigned LEB128. This indicates which column in the frame table represents the return address.

The next fields of the CIE depend on the augmentation string.

    If the augmentation string starts with ‘z’, we now find an unsigned LEB128 which is the length of the augmentation data, rounded up so that the CIE ends on an address boundary. This is used to skip to the end of the augmentation data if an unrecognized augmentation character is seen.
    If the next character in the augmentation string is ‘L’, the next byte in the CIE is the LSDA (Language Specific Data Area) encoding. This is a DW_EH_PE_xxx value (described later). The default is DW_EH_PE_absptr.
    If the next character in the augmentation string is ‘R’, the next byte in the CIE is the FDE encoding. This is a DW_EH_PE_xxx value. The default is DW_EH_PE_absptr.
    The character ‘S’ in the augmentation string means that this CIE represents a stack frame for the invocation of a signal handler. When unwinding the stack, signal stack frames are handled slightly differently: the instruction pointer is assumed to be before the next instruction to execute rather than after it.
    If the next character in the augmentation string is ‘P’, the next byte in the CIE is the personality encoding, a DW_EH_PE_xxx value. This is followed by a pointer to the personality function, encoded using the personality encoding. I’ll describe the personality function some other day.

The remaining bytes are an array of DW_CFA_xxx opcodes which define the initial values for the frame table. This is then followed by DW_CFA_nop padding bytes as required to match the total length of the CIE.

An FDE starts with the length and ID described above, and then continues as follows.

    The starting address to which this FDE applies. This is encoded using the FDE encoding specified by the associated CIE.
    The number of bytes after the start address to which this FDE applies. This is encoded using the FDE encoding.
    If the CIE augmentation string starts with ‘z’, the FDE next has an unsigned LEB128 which is the total size of the FDE augmentation data. This may be used to skip data associated with unrecognized augmentation characters.
    If the CIE does not specify DW_EH_PE_omit as the LSDA encoding, the FDE next has a pointer to the LSDA, encoded as specified by the CIE.

The remaining bytes in the FDE are an array of DW_CFA_xxx opcodes which set values in the frame table for unwinding to the caller.

The DW_EH_PE_xxx encodings describe how to encode values in a CIE or FDE. The basic encoding is as follows:

    DW_EH_PE_absptr = 0x00: An absolute pointer. The size is determined by whether this is a 32-bit or 64-bit address space, and will be 32 or 64 bits.
    DW_EH_PE_omit = 0xff: The value is omitted.
    DW_EH_PE_uleb128 = 0x01: The value is an unsigned LEB128.
    DW_EH_PE_udata2 = 0x02, DW_EH_PE_udata4 = 0x03, DW_EH_PE_udata8 = 0x04: The value is stored as unsigned data with the specified number of bytes.
    DW_EH_PE_signed = 0x08: A signed number. The size is determined by whether this is a 32-bit or 64-bit address space. I don’t think this ever appears in a CIE or FDE in practice.
    DW_EH_PE_sleb128 = 0x09: A signed LEB128. Not used in practice.
    DW_EH_PE_sdata2 = 0x0a, DW_EH_PE_sdata4 = 0x0b, DW_EH_PE_sdata8 = 0x0c: The value is stored as signed data with the specified number of bytes. Not used in practice.

In addition the above basic encodings, there are modifiers.

    DW_EH_PE_pcrel = 0x10: Value is PC relative.
    DW_EH_PE_textrel = 0x20: Value is text relative.
    DW_EH_PE_datarel = 0x30: Value is data relative.
    DW_EH_PE_funcrel = 0x40: Value is relative to start of function.
    DW_EH_PE_aligned = 0x50: Value is aligned: padding bytes are inserted as required to make value be naturally aligned.
    DW_EH_PE_indirect = 0x80: This is actually the address of the real value.

If you follow all that, and also read up on .debug_frame, then you have enough information to unwind the stack at runtime, e.g. to implement glibc’s backtrace function. Later I’ll describe the LSDA and the personality function, which work together to implement exception catching on top of stack unwinding.

Traces
------

Most operations on an error will produce another error, e.g. ``case {}.x of 1 -> ...`` produces ``MissingCaseError (NoSuchAttributeError ...)``. So the error bubbles up until we get something that has a catch-all to handle errors, e.g. the main program handler that prints the error and exits. With fancy formatting the nested errors will look like a stacktrace - but the stack is the stack of future operations, rather than where the program has been.

We can redefine this error value to be something else, e.g. add a definition ``NoSuchAttributeError {} "x" = 3``. Then ``{}.x == 3`` and the error is silenced. Similarly we can do ``case {}.x of NoSuchAttributeError {} "x" -> 3``, or pass the error to a function that does such error-handling.

The errors can also keep track of their continuation, e.g. a ``MissingCaseError`` can store its continuation ``\x -> case x of ...``. These compose up the stack so that we can pass in a value at any point and resume computing.

State
-----

For a stateful function, the ``RealWorld`` token also is replaced with an error value. So no further states can be executed until the error is handled. But the error value itself contains a new ``RealWorld`` token to allow resuming the computation. We can define the standard levels of safety: no-throw is that the normal state will be returned, strong exception safety of a function is the assertion that the state in the error value is no different from the state passed in, and basic safety is that all documented invariants are maintained for the state in the error value. Most operations with basic safety can be made strongly safe by copying all relevant data beforehand, besides actual I/O operations.

try-catch-else-finally: we can handle the try-catch part with continuations and the error-redefining trick, ``case reset (Left (foo {e | isDesiredError e = shift (const e)}) of e | isDesiredError e -> handle e``. We can also use the bubbling: ``case x of e | isError e and isDesiredError (firstError e) -> ...``. For finally we want a state field to extract the token, ``case x of e -> e { state = cleanup (state e) }``. Python also supports an else clause - it is executed if control flows normally off the end of the try clause and is not protected by the catch clauses of the try.

asynchronous exceptions: this instruments every memory allocation and I/O operation to check for calls to ``throwTo ThreadId`` and if so return ``Interrupted``, ``ThreadKilled`` (``PleaseStop``), etc. But every operation is also given a parameter ``Masked`` (for memory and nonblocking I/O operations) or ``Interruptible`` (for blocking I/O operations) that disables this behavior. Then there's the mask function, ``mask io = if Masked then io {unmask = id} else io {Masked = True, unmask io = io {Masked = False} }`` and similarly ``uninterruptibleMask`` which also checks/sets ``Interruptible``.

Concurrency
===========

Concurrency is the ability to execute units of a program in varying orders. Generally this is done for performance, so we want to verify that the order does not affect the output of the program, i.e. there are no race conditions. To this end we need to specify which outputs are equivalent, which can be accomplished by applying a ``deterministic`` predicate to important outputs, and also the allowed/possible execution orders.

An order is usually defined as a linear order. But if we consider from a physics point of view it is more complicated, event separation can be timelike or spacelike. So really we want to use a partial order. Hence an execution produces a directed graph of local states, where :math:`\to` is read "can casually influence". We can annotate the arrows with the information passed, and take the transitive closure to get a poset.

So an execution forms a poset - but often execution is nondeterministic. So in general the possible executions form a set of posets. In the least tractable case this set is arbitrary and the verifier must check all possible orderings. But if we assume that events can be independent, i.e. for specific events a,b reordering ...a b... to ...b a... and vice-versa does not change the behavior or whether the ordering is allowed, then the problem can be reduced to checking a set of posets. The posets are more resistant to state space explosion.

Various synchronization primitives:

* Linux kernel internal operations: `model <https://github.com/torvalds/linux/blob/3d5c70329b910ab583673a33e3a615873c5d4115/tools/memory-model/linux-kernel.def>`__ `atomic x86 operations <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/x86/include/asm/atomic64_64.h>`__ `lock types <https://www.infradead.org/~mchehab/kernel_docs/locking/locktypes.html>`__
* atomic operations
* memory barrier
* spin lock



yes, SA's (and KSA's) are an interesting concept, but i personally think
they are way too much complexity - and history has shows that complexity
never leads to anything good, especially not in OS design.

Eg. SA's, like every M:N concept, must have a userspace component of the
scheduler, which gets very funny when you try to implement all the things
the kernel scheduler has had for years: fairness, SMP balancing, RT
scheduling (!), preemption and more.

Eg. 2.0.2 NGPT's current userspace scheduler is still cooperative - a
single userspace thread burning CPU cycles monopolizes the full context.
Obviously this can be fixed, but it gets nastier and nastier as you add
the features - for no good reason, the same functionality is already
present in the kernel's scheduler, which can generally do much better
scheduling decisions - it has direct and reliable access to various
statistics, it knows exactly how much CPU time has been used up. To give
all this information to the userspace scheduler takes alot of effort. I'm
no wimp when it comes to scheduler complexity, but a coupled kernel/user
scheduling concept scares the shit out of me.

And then i havent mentioned things like upcall costs - what's the point in
upcalling userspace which then has to schedule, instead of doing this
stuff right in the kernel? Scheduler activations concentrate too much on
the 5% of cases that have more userspace<->userspace context switching
than some sort of kernel-provoked context switching. Sure, scheduler
activations can be done, but i cannot see how they can be any better than
'just as fast' as a 1:1 implementation - at a much higher complexity and
robustness cost.

the biggest part of Linux's kernel-space context switching is the cost of
kernel entry - and the cost of kernel entry gets cheaper with every new
generation of CPUs. Basing the whole threading design on the avoidance of
the kernel scheduler is like basing your tent on a glacier, in a hot
summer day.

Plus in an M:N model all the development toolchain suddenly has to
understand the new set of contexts, debuggers, tracers, everything.

Plus there are other issues like security - it's perfectly reasonable in
the 1:1 model for a certain set of server threads to drop all privileges
to do the more dangerous stuff. (while there is no such thing as absolute
security and separation in a threaded app, dropping privileges can avoid
certain classes of exploits.)

generally the whole SA/M:N concept creaks under the huge change that is
introduced by having multiple userspace contexts of execution per a single
kernel-space context of execution. Such detaching of concepts, no matter
which kernel subsystem you look at, causes problems everywhere.

eg. the VM. There's no way you can get an 'upcall' from the VM that you
need to wait for free RAM - most of the related kernel code is simply not
ready and restartable. So VM load can end up blocking kernel contexts
without giving any chance to user contexts to be 'scheduled' by the
userspace scheduler. This happens exactly in the worst moment, when load
increases and stuff starts swapping.

and there are some things that i'm not at all sure can be fixed in any
reasonable way - eg. RT scheduling. [the userspace library would have to
raise/drop the priority of threads in the userspace scheduler, causing an
additional kernel entry/exit, eliminating even the theoretical advantage
it had for pure user<->user context switches.]

plus basic performance issues. If you have a healthy mix of userspace and
kernelspace scheduler activity then you've at least doubled your icache
footprint by having two scheduler - the dcache footprint is higher as
well. A *single* bad cachemiss on a P4 is already almost as expensive as a
kernel entry - and it's not like the growing gap between RAM access
latency and CPU performance will shrink in the future. And we arent even
using SYSENTER/SYSEXIT in the Linux kernel yet, which will shave off
another 40% from the syscall entry (and kernel context switching) cost.

so my current take on threading models is: if you *can* do a really fast
and lightweight kernel based 1:1 threading implementation then you have
won. Anything else is barely more than workarounds for (fixable)
architectural problems. Concentrate your execution abstraction into the
kernel and make it *really* fast and scalable - that will improve
everything else. OTOH any improvement to the userspace thread scheduler
only improves threaded applications - which are still the minority. Sure,
some of the above problems can be helped, but it's not trivial - and some
problems i dont think can be solved at all.

But we'll see, the FreeBSD folks i think are working on KSA's so we'll
know for sure in a couple of years.



The Go language is generally safe, in that errors in your program do not lead to unpredictable crashes (unless you use the facilities in the unsafe package). There is one classic cause of problems, however, which Go does not protect you from: race conditions. A race condition occurs when one goroutine modifies a variable and another reads it or modifies it without any synchronization.

Go makes correct synchronization easy, following the slogan of “do not communicate by sharing memory; instead, share memory by communicating.” Race conditions only occur when sharing memory. However, although correct synchronization is easy, the language does not enforce it. Race conditions are possible and I have seen them in real Go programs.

Is it possible for a language to be safe with regard to race conditions? In a language that supports multiple threads of execution, it’s always going to be possible to construct race conditions by doing things like having multiple threads write to the same offset in the file. Let’s rule out that somewhat exotic variety and concentrate on race conditions which don’t involve external constructs. Let’s also not worry about other threading problems like deadlock or starvation; they are real problems, but they are usually easier to understand and avoid.

Race conditions in memory always involve global variables, in which I include memory which can be accessed via a pointer. When more than one thread can access a global variable, the variable can become a nonlocal side effect: something which can affect program behaviour while changing in a way that is not obvious from reading the program code. When that variable can change without any synchronization, you may have a race condition.

One approach, then, is to prohibit variable modifications. When no variables can be modified, no race conditions are possible. This may sound odd to programmers used to conventional imperative languages like C++, Java, or Go, but this is the approach used by strict functional languages. There are few truly strict functional languages outside of academia, but languages like Haskell approach the goal by clearly delineating the non-strict set of operations. Functional languages generally do not have explicit threading models, but the lack of side effects makes it possible to automatically parallelize them. E.g., when a function is called with two arguments, both arguments can be evaluated in parallel; since there are no global variables, there is no way that the evaluation of one argument can affect the evaluation of the other.

While functional languages have some clear benefits, I think they face a problem of history. They’ve been around for a long time, but they are not widely used. It’s possible that this is merely a matter of education, but I think it’s a more fundamental issue. The real world is full of global state and non-local side effects. I think it’s hard for programmers to adapt to languages which don’t support them. Operations that are conceptually very simple, like sorting a list of numbers, become harder to understand in a functional language. Functional languages will always have adherents and will always have good uses in specific domains, but I think widespread adoption is unlikely. Go is obviously not a functional language.

A somewhat less strict approach for avoiding race conditions is for different threads of execution to not share memory. Without shared memory, all thread communication must use a limited set of communication operations which can avoid races. The Erlang language follows this model. Erlang is a functional language, though somewhat less strict than Haskell. Erlang has an explicit thread model. However, threads do not share memory, and can only communicate via message passing. In effect this enforces Go’s slogan: all sharing must be done by communication. This model also has a nice advantage in that threads can run on the same machine or a different one with no change in the program, but only a change in efficiency.

Go did not adopt this model because of efficiency considerations. Threads communicating in shared memory are far more efficient that threads communicating via message passing. In shared memory you can pass around pointers to large data structures. Without shared memory, large data structures must be sent as a whole. Real implementations of Erlang have a variety of implementation optimizations to reduce the cost of sending large data structures, but that means that programmers have to be aware of the available optimizations when writing programs.

One could argue that correct program behaviour is always more important than efficiency, and that the efficiency advantages of shared memory should be disregarded in the name of correctness. However, that argument does not hold up in the real world, where efficiency does matter. Erlang made the right choice for an environment in which threads can run on different machines and can even move from one machine to another. Go may not be the right choice for that environment. Of course, it is also not the environment in which most programs run.

I don’t know of any other approaches to avoiding race conditions in a programming language, though I’m sure there are some. One possibility would be for the language to maintain a global synchronization state. Each operation on a global variable would then be annotated with the current state. If two operations were done using an incompatible state, meaning operations done by different threads with no explicit synchronization, you would get an error.

If this were done purely dynamically, the result would be similar to Thread Sanitizer or helgrind. With language support, it could be constructed to avoid all race conditions. However, since race conditions would only be detected dynamically, it would mean that your program could still crash occasionally at runtime. This would be much better than the current situation, in which your program can behave unpredictably at runtime. The dynamic information would also provide a lot of the information required to debug the problem.

This approach could also be used statically, but I suspect it would require fairly extensive annotations on functions to indicate their synchronization status. It might be possible to deduce some of that statically using whole program analysis, but in a language like Go with channels I think that would be difficult in practice and impossible in theory (i.e., some programs would provably require annotations, and most programs would in practice require annotations).

So what would the dynamic approach look like? It would basically mean instantiating the Go memory model in each operation, tracking the happens before relationships. For example, give every goroutine a ticker. When a goroutine sends a value on a channel or acquires or releases a lock, increment the ticker. When sending a value on a channel or releasing a lock, attach the current goroutine and its tick. When receiving a value on a channel or acquiring a lock, record a happens-before relationship from the attached goroutine/ticker to the current goroutine/ticker. When modifying any global variable, including any shared memory, annotate the variable with the goroutine and its ticker. When reading any global variable, look at the current annotation, and require that there be a happens-before relationship between that annotation and the executing goroutine.

What’s nice about this approach is that it doesn’t require any change to the language or to your program. What’s not nice is that you have to pay a heavy execution cost, and you only catch all actual race conditions, you don’t catch all possible race conditions. It would be nice if the heavy execution cost could be mitigated by static analysis, but in general I don’t think it can. Escape analysis can prove that some variable reads don’t require checking annotations, but I think that even simple programs will defy such analysis for most reads.

On the other hand, with some caching, perhaps the execution cost would not be prohibitive. Most reads are going to be of values written by the current goroutine. That requires an extra read and comparison for each memory read, which would be bad but perhaps supportable. Since one would presumably want to track individual memory words, memory usage would double. Alternatively, since the Go memory model is written in terms of variables, one could instead have an extra word per variable. Then pointer accesses would require identifying the appropriate variable, even if the pointer were to the middle of the variable.

I doubt this could ever be the default way to run Go, but it would certainly be interesting to try an implementation and find the real performance effects.
