Time API
########

Instant
=======

An instant marks a proper time (real number) as measured by some observer.  Although relativity means not all events can be given a time,

A timescale T is a one-to-one function of the proper time, i.e. each instant corresponds to a value on the timescale.

::

  set Instant

  timescale subset Instant

In Python's terminology a timescale value is "aware", but Python also considers bare times like ``10:34 AM`` with a UTC offset to be aware, which is pretty weird because you can't convert it to a proper time without the day.

Timescales can be converted to each other, just convert to and from proper time or (practically) to and from a well-known timescale such as TAI.

Time standards
==============

* International Atomic Time (TAI)
* Terrestrial Time (TT)
* GPS time (GPS)
* Barycentric Coordinate Time (TCB)
* Geocentric Coordinate Time (TCG)
* Barycentric Dynamical Time (TDB)
* Universal Time (UT1)
* True solar time at prime meridian (TST)
* Local true solar time at longitude l (LTST l)
* UTC
* UTC offset UTC±[hh]:[mm], ``Offset = { hours : range_exclusive -24 24, minutes : range 0 60}``
* Civil timezone with ``TimeZoneId = { id : String | lookupZoneRules id : ZoneRules }``
* Google/AWS smeared UTC
* UTC-SLS or UTS smeared UTC
* Bloomberg smeared UTC

A standard defines a way of marking time, including the length of a second and of a day. The length can vary, making a nonlinear transformation to proper time. TCG, TT, TAI, GPS, UT1 and TST have a fixed day length of 86400 seconds; for UT1/TST it is one rotation of the Earth, while for TCG/TT/TAI/GPS it is simply a nominal day. The nominal day is shorter than the time for a full Earth rotation, and it is somewhat misleading to use such day-based notations because they are not connected to the solar day, but it is conventional practice nonetheless. UTC uses a varying day of 86400 plus or minus a second. Civil time varies like UTC and also has daylight savings time and other zone transitions.

For measuring durations TAI, TT, or GPS is best because the second is uniform and based on the SI definition. Although the system's CLOCK_MONOTONIC is close, NTP's corrections become important over longer time scales. NTP errors can be between 2ms-100ms depending on connectivity. For astronomical calculations UT1, TST, and LTST are best. UTC's only real advantage is its ubiquity - there are lookup tables (leap second table, DUT1) and formulas to convert UTC to other formats. TCG, TCB, TDB are rare formats and do not necessarily need to be implemented immediately.

Epochs
======

The simplest method of specifying an instant is to give it a name. A named instant is called an epoch. There are numerous epochs, too many to list here. The epochs may be calendar times which require a time standard to interpret as an instant, such as Rata Die, or be a fixed instant in a given standard, such as the GPS epoch.

::

  CalendarEpoch = Standard -> Instant
  FixedEpoch = Instant

Epoch-relative time
===================

The form of specifying time most convenient for calculations is as a quantity of time elapse relative to some particular epoch, where the quantity is a number and some time unit. The time standard must also be specified to fix the length of the day and the second. So it looks as follows:

::

  EpochTime = { offset : real, unit : ChronoField, epoch : Epoch, standard : TimeStandard }

  EpochTime 14247 day UnixEpoch UTC -- 14247 days after Unix epoch

The unit is a singular ChronoField, because a real number times one field is sufficient to express any instant. For multiple fields use a time record or tuple of time duration and epoch.

Epoch times using days as the unit are called epoch dates. JD and MJD are common epoch date formats.

Time record
===========

A time record specifies an instant using a collection of fields from the proleptic Gregorian calendar (ISO 8601), implicitly using the Rata Die epoch. The time standard must also be specified.

::

  AdjustField = {InLeapSecond, TAIOffset, InDST, UTCHourOffset, Fold}

  ChronoField = {Nanosecond, Microsecond, Millisecond, Second, Minute, Hour, Half_Day, Day, Week, AlignedWeek, Month, Quarter, Year, JulianAstroYear, BesselianAstroYear, Decade, Century, Millennium, Era} union AdjustField


  -- convertible to integers
  DayOfWeek = Monday through Sunday
  Month = January through December
  Era = BC | AD

  TimeRecord = { fields : Map ChronoField (Real|DayOfWeek|Month|Era), standard : Standard }

  NormalizedTimeRecord subset TimeRecord

An un-normalized time record may have any real value for any of the fields, except for era for which only two values are defined (similar to a sign bit). Normalization calculates an instant as the sum associated left from least precise to most precise, e.g. ``{ year = 2006.6, month = 0, day = 3 }`` gives ``((RataDie + 2006.6*year)+0*month)+3*day``.

After normalization the range and type of the fields of the result depend on which fields were included. The most precise field is a real in some range, while the other fields are integers, and the least precise field besides era is unbounded. There are also special types for some fields. And some fields are conventionally 1-indexed. The BC era is numbered backwards. Some examples:
* year : int, month : Month, day : range int 1 32, hour : range real 0 24
* year : int, day : range real 1 367
* year : int, week : range int 1 54, day : DayOfWeek
* year : int, alignedweek : range int 0 53, day : range real 0 7
* decade : int, year : range int 0 10, day : range real 1 367
* century : int, decade : range int 0 10, year : range int 0 10, day : range real 1 367
* century : real
* day : real
* era : Era, year : range real 1 infinity

Since the range and type varies there is an API to compute it:

::

  NormalizedRange = { minSmallest : real, maxSmallest : real, minLargest : real, maxLargest : real
    | minSmallest <= minLargest <= maxLargest && minSmallest <= maxSmallest <= maxLargest }

  -- only looks at which fields are present
  range : Set ChronoField -> Map ChronoField NormalizedRange
  type : Set ChronoField -> Map ChronoField {Int,Real,DayOfWeek,Month,Era}

  -- looks at values of fields
  preciseRange : TimeRecord -> Map ChronoField (min : real, max : real)

For the range, all values are within ``[minSmallest, maxLargest)``, but setting a field to a value outside of ``[minLargest, maxSmallest)`` may produce an un-normalized value. To avoid this the precise range can be used, but it is dependent on the values of the less-precise fields (e.g. number of days in a month varies based on the month and year).

Regarding leap seconds in UTC, if none of the adjustment fields are specified, then seconds can have an integer part of 60, representing the leap second. With in_leap_second, it goes ``[(59,false),(0,true),(0,false)]``. Similarly fold represents ambiguous times with 0=the earliest time, 1=the next earliest, etc.

For civil time, there is also DST. Without adjustment fields this is represented by varying the number of hours in the day - useful for calculations, but probably not desired for display. InDST represents this unambiguously, or fold may be used - it takes on the value 2 if DST and the leap second overlap (unlikely).

Other calendars
---------------

There are other calendars, e.g. the Julian calendar and localized calendars like Hijrah, Thai Buddhist, Japanese, and Minguo. These can have their own ChronoFields like ``Julian_year``, ``Japanese_era``, etc. so there should be a way to extend the list of ChronoFields.

Duration
========

A duration identifies a time difference as a collection of time fields. All the fields are real:

::

  Duration = { fields : Map ChronoField Real, standard : Standard }

The length of the fields varies by time standard, so that must also be specified. Although the primary use of a duration is to be added or subtracted from an instant, and some instant formats have standards associated with them which might allow avoiding the standard field in the duration, other instant types such as well-known fixed instants have no standard, so the instant's standard cannot be used in general.

Partial date
============

A partial date is just a collection of time fields, and a list of all of them for the complete date. There is an "update" operation which takes an instant and a partial date that has the semantics of replacing or overriding the fields of the instant with those of the partial date's, when the instant is expressed as a time record in the given standard.

::

  PartialDate = { complete : Set ChronoField, fields : Map ChronoField (Int|Real|DayOfWeek|Month|Era), standard : Standard }

For example a POSIX timestamp is a partial date with its InLeapSecond field unspecified.

Intervals
=========

Time intervals represent a closed interval ``[s,e]`` of instants.

::

  TimeInterval = { start : Instant, end : Instant} -- inclusive

Probably the standard interval type is sufficient.

Recurrence
==========

This represents a recurrence rule as documented in the iCalendar RFC. (c.f. https://dateutil.readthedocs.io/en/stable/rrule.html)

::

  rrule =
    { dtstart : datetime
    , freq : {YEARLY, MONTHLY, WEEKLY, DAILY, HOURLY, MINUTELY, SECONDLY}
    . interval: int
    , (count : int | until : datetime)
    , wkst : DayOfWeek
    , rules : Map {bysetpos, bymonth, bymonthday, byyearday, byweekno, byweekday, byhour, byminute, bysecond, byeaster} [int]
    }

Format information
==================

This is used for pretty-printing and parsing instants.

::

  DateTimeFormat =
    { printerParser : CompositePrinterParser
    , locale : Locale
    , decimalStyle : DecimalStyle
    , resolverStyle : ResolverStyle
    , resolverFields : Set ChronoField
    , standard : Standard
    }
  FormatStyle = enum { Full, Long, Medium, Short }
  ResolverStyle = enum {Strict, Smart, Lenient}
  -- strict: reject invalid values
  -- smart: day-of-month 32 is clipped to day 30 or 31
  -- lenient: month 15 is treated as being 3 months after month 12.
  SignStyle = enum {Normal, Always, Never, Not_Negative, Exceeds_Pad}
  -- Normal: -1, 2
  -- Always: -1, +2
  -- Never: 1, 2
  -- Not_Negative: exception, 2
  -- Exceeds_Pad: Normal but output the sign if the value exceeds the pad width.
  -- In lenient parsing, any sign will be accepted unless the width is fixed,
  -- with the absence of a sign treated as a positive number.
  TextStyle = enum {Full, Full_Standalone, Short, Short_Standalone, Narrow, Narrow_Standalone}
  -- standalone does not apply to English, but in other languages you have "MarchX" and "MarchY 2, 2003"

Time zones
==========

Civil time standards refer to a time zone database for the UTC offset using a time zone ID string.

A time zone is a list of transitions:

::

  ZoneOffsetTransition =
    { instant : Instant
    , offsetBefore : Offset
    , offsetAfter : Offset
    }

At the instant given the offset changes from before to after.

Most transitions follow a regular pattern, so are given by a rule:

::

  ZoneOffsetTransitionRule =
    { month : Month, dayOfMonth : i8, dow : DayOfWeek, time : LocalTime, timeEndOfDay : boolean
    , timeDefinition : TimeDefinition, standardOffset : ZoneOffset
    , offsetBefore : ZoneOffset
    , offsetAfter : ZoneOffset
    }

So overall Java uses this thing for a time zone:

::

  ZoneRules =
    { standardTransitions : [Instant]
    , standardOffsets : [ZoneOffset] | length standardOffsets = length standardTransitions + 1
    , savingsLocalTransitions : [LocalDateTime] | length savingsLocalTransitions = length wallOffsets * 2
    , wallOffsets : [ZoneOffset]
    , savingsInstantTransitions : [i64] | length savingsInstantTransitions = length wallOffsets
    , lastRules : [ZoneOffsetTransitionRule]
    }

TODO: Checkout Python zoneinfo, whatever Rust does, etc. for a better API

The JSR-310 `ThreeTen <https://www.threeten.org/>`__ library in `Java <https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/package-summary.html>`__ seems to have undergone the most peer review. It relies heavily on ISO 8601. Another is http://time4j.net/. For some reason these are all Java. Rust has a basic thing in `std <https://doc.rust-lang.org/std/time/index.html>`__. There is a more complete Rust library but the author is `opinionated <https://github.com/time-rs/time/issues/406#issuecomment-989753838>`__.


OS interface
============

A few functions:
* guess civil time standard of the system (it's a guess because the system could be crazy)
* get the current system time as an instant (possible because OS's have leap second APIs to allow converting to TAI)

System time formats are generally instants, but some don't specify enough information or are durations.
