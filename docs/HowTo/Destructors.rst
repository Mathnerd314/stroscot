Using destructors
#################

AutoCloseFD
===========

An example usage of destructors is based on `AutoCloseFD <https://android.googlesource.com/platform/system/vold/+/android-7.1.1_r11/AutoCloseFD.h>`__. ``AutoCloseFD`` is an internal symbol so as to strengthen encapsulation and avoid separating the destructor and the file descriptor.

::

  open path mode flags =
    fd = sys_open(path, flags | O_CLOEXEC, mode)
    AutoCloseFD fd newDestructor

  maybeClose (AutoCloseFD fd d) = if lastUse d then sys_close fd else {}

  write t@(AutoCloseFD fd _) ... =
    sys_write fd ...
    maybeClose t

malloc/free can be similarly used.

Time travel
===========

The determination of which lastUse call is actually the last use is somewhat complex, but for non-branching control flow it's easy to follow. With branches it is possible to get time-travel errors, for example

::

  d = newDestructor
  if lastUse d
    print "Contradiction!"
    lastUse d

In a lot of cases there is no observable difference in which lastUse returns true. E.g. just removing the print statement from the example we'd have nothing depending on lastUse and it can be eliminated. Similarly for ``maybeClose; if ... { maybeClose }`` the control flow is equivalent to having a lastUse at the end outside the conditional. So hopefully time travel errors will remain curiosities.

