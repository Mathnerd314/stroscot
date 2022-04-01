Using destructors
#################

General principle: Use a destructor or finalizer whenever you have an allocate-clean up pair. Use a destructor if you want the guarantee that cleanup is called immediately after an operation, otherwise use a finalizer.

AutoCloseFD
===========

An example usage of destructors is based on `AutoCloseFD <https://android.googlesource.com/platform/system/vold/+/android-7.1.1_r11/AutoCloseFD.h>`__. ``AutoCloseFD`` is not exported so as to strengthen encapsulation and in particular to avoid separating the destructor and the file descriptor.

::

  module AutoCloseFD hiding AutoCloseFD

  symbol AutoCloseFD

  open path mode flags =
    fd = sys_open(path, flags | O_CLOEXEC, mode)
    AutoCloseFD fd newDestructor

  maybeClose (AutoCloseFD fd d) = if !(lastUse d) then sys_close fd else {}

  write t@(AutoCloseFD fd _) ... =
    sys_write fd ...
    maybeClose t

  ... more operations on file descriptors ...

malloc/free can be similarly used.

To interface code using manual management with code using destructors there are two choices. You can use the destructor version as the underlying object and make the manual methods use the destructor and ``close`` a no-op or a call to ``use`` (the latter preserves closing behavior if the destructor code does not free the resource). Or you can make the manual version the underlying object and add a variable to track if the resource has been freed, erroring in all the destructor operations if the resource is freed.

Time travel
===========

The determination of which lastUse call is actually the last use is somewhat complex, but for non-branching control flow it's easy to follow. With branches it is possible to get time-travel errors, for example

::

  d = newDestructor
  if lastUse d
    print "Contradiction!"
    lastUse d

In a lot of cases there is no observable difference in which lastUse returns true. E.g. just removing the print statement from the example we'd have nothing depending on lastUse and it can be eliminated. Similarly for ``maybeClose; if ... { maybeClose }`` the control flow is equivalent to having a lastUse at the end outside the conditional. This is why using finalizers is in most cases the correct choice. The finalizer is called only once and cannot invalidate itself.

Free on exit
============

Since program resources are freed on exit by the operating system, we can speed up the program by not running destructors and finalizers before exit. This amounts to calling ``use`` after the exit. Not sure how this interacts with exit finishing the program.
