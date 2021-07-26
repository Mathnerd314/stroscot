Transactions
############

With transactions we can write various common synchronization constructs.

Locks
-----

::

  l = mem false
  acquire l =
    atomically
      if(l)
        retry
      l := true
  release l =
    atomically
      assert(l)
      l := false
  isHeld l = atomically { l }

Semaphore
---------

::

  s = mem 0
  wait s =
    atomically {
      if s == 0
        retry
      s := s - 1
    }
  signal s =
    atomically {
      s := s + 1
    }
  read s =
    atomically {
      s
    }

A `bounded semaphore <https://docs.python.org/3/library/threading.html#threading.BoundedSemaphore>`__ is similar:

::

  signal_bounded =
    atomically {
      s := s + 1
      assert(s <= bound)
    }

Condition variable
------------------

::

  wait p l action =
    atomically
      lock l
      if not p
        retry
      action
      unlock l

General idea
------------

Running transactions on an event loop looks like:

::

  newRequests = queue []
  allWrites = []
  for(r in requests_in_nondet_order)
    case r of
      ...
      Transaction t next =
        writes = []
        finished = run (t Done)
          where
            run Done = return true
            run Retry = return false
            run (Read c f) =
              v = lookup map c
              run (f v)
            run (Write c v f) =
              writes.push((c,v))
              run f
        if finished && (executeThisCycle = nonDetBool) && noConflicts(writes,allWrites)
          allWrites.append(writes)
          newRequests.push(next)
        else
          newRequests.push(r)
  for (c,v) in allWrites
    c := v

