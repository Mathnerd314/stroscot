Units
#####

Fortress, Julia, and Python all have unit libraries. So it seems worthwhile to implement these. Although quantities with units will probably never be the default values used by APIs, units are useful for safety.

 handling exponents and other dimensionless operations is an issue.

Quantities
==========

A quantity is a value represented by a number and a unit, ``Quantity num unit``. For example "10 avoirdupois pounds", "10 lbs", "160 ounces" are all (distinct) quantities. The syntax is juxtaposition like ``1 kg`` or an infix "unit" operator ``2 unit (kg^2)`` for more complicated expressions. A pure number is a dimensionless quantity ``x unit 1``, convertible to the raw numeric type.

The number is usually a real number, but a vector or a complex number can be combined with a unit as well. A vector quantity is distinct from a vector of scalar quantities all with the same unit (but is interconvertible, discussed next) You can also have a vector of quantities with different units, e.g. ``(1 m, 2 s)``. Some units may technically not be defined for negative numbers, e.g. mass, but in practice using real numbers makes computation much more convenient. Also, complex number must have split units, as when you take the log of a pure complex quantities you get a real part in nepers and an imaginary part in radians.

The number must be represented in some format, so quantities are further divided according to their number's format, e.g. a ``(10:float32) meter`` is distinct from a ``(10:float64) meter``. You can have special formats, e.g. binary radians are a format ``{0 .. 2^(n−1)}*2*pi/2^n`` combined with the radian unit. As with all formats there are restrictions on the range and precision of numbers that may be represented.

An expression like "10 pounds" may be interpreted as multiple quantities, e.g. "10 avoirdupois pounds" (a weight) or "10 British pounds" (a currency), depending on context. The context could be represented via an implicit argument or whatever, but it seems a lot easier to forbid ambiguity and devise a coherent set of unit names, e.g. Frink's "pound is weight and GBP is currency". So expressions are required to be unambiguous.

Units
=====

Units that seem worth supporting:
* The 7 SI base units, like second, meter, kilogram
* Derived units, like feet, kilometers, newtons, etc., and their SI prefixes
* Radians, see :cite:`quinceyImplicationsAdoptingPlane2016` or https://arxiv.org/pdf/2203.12392.pdf for how formulas adjust to accommodate angles as a dimension
* Information units nat and bit
* Counting units, molecules, neutrons, atoms, cycles, and so on
* Substance units, derived unit plus substance name, e.g. g NaCl or g salt
* Dimensioned logarithmic units, ``f * log_b(x/r)``, where factor f, reference quantity r (with unit), and log base b are known. (dBm, dBV, dBSPL, pH, Richter, dB_power(r) and dB_root_power(r) for a reference quantity r such as 1 km)
* Dimensionless logarithmic units, ``f * log_b(x1/x2)``, similar but ``x2`` is not part of the unit (dB, Np - but again with the distinction between power and root power. also decades, octaves, semitones, stops)

The are all multiplicative units, meaning a product of integer powers of units is a new multiplicative unit.

Then there are affine units, which are a scale and offset referenced to a derived unit. Examples are Celsius referenced to Kelvin, Fahrenheit referenced to Rankine, timescales to second, dates to day, locations to 3D vector of kilometers. A GCS coordinate such as ``(17.8416656,-124.0563834) WGS 84`` describes the location on the surface of the ellipsoid, which can be subtracted from "Null Island" at 0,0 or the center of the earth to obtain a 3D vector.

A unit like ``degF / kg`` as used in old scientific work doesn't mean the affine Fahrenheit scale, instead ``degF`` is interpreted as a temperature difference. This could be hacked in, but similar to pounds weight vs. pounds currency, it is easier to make it unambiguous by requiring ``degR / kg`` in this context and forbidding combining affine units with other units. Rankine has the same difference size as Fahrenheit and since it's referenced to absolute zero it scales properly.

Percentage seems like it should be a unit, similar to dimensionless logarithmic units but less well-behaved. Hence we forbid combining percentage units with other units.

As far as parts per million (ppm), ppb, ppt, etc., Wikipedia has a `handy conversion table <https://en.wikipedia.org/wiki/Parts-per_notation#SI-compliant_expressions>`__ to SI notation, and the SI notation seems much clearer. Furthermore with substance units the SI-like ratio doesn't collapse to a dimensionless value, whereas ppm is ambiguous. So writing it out seems better.

So in the end the units we have are multiplicative units, affine units, and percentages.

Subdivision units
=================

These are the same dimension as the base unit but are expressed with numbers representing finer subdivisions. For example DMS notation 1 degree 1 minute 1 second, equal to 1 + 1/60 + 1/360 degrees, or 6 feet 2 inches, equal to 74 inches.

Accessors
=========

Stripping a quantity gives the bare number, e.g. ``strip (2 mm) = 2``. It's safer to write the unit you expect to be stripped, ``strip x mm``, which fails or converts the unit if it doesn't match. This together with the ``Quantity`` constructor allows interoperating with non-unit-aware APIs. You can recursively strip, which removes units in vectors, ``rstrip (2 mm, 3 kg) = (2,3)``. Similarly you can get the unit, ``unit (2 mm) = mm``.

There is also a ``strip_substance`` function, similar to strip but used for cancelling ``mol A / mol B`` to ``mol/mol = 1`` when you want a pure number.

Conversions
===========

We want to convert quantities to other units, ``convert quantity unit`` which expands to ``convert from_value from_unit to_unit``. The desired result format should also be specified if it is not obvious, ``convert quantity unit {format=Guess}``. Actually the type may contain embedded dimensional units, so it should be ``convert quantity type``. For example distributing units in a vector is ``convert ((1,2,3) m) [Real unit Meter]  = (1 m, 2 m, 3 m)``. Or infix ``(1,2,3) m to [Real unit Meter]``.

The conversion should use the most precise calculation possible, passing the formula through a floating-point accuracy tool like Herbie, but in practice it seems libraries implement conversions by converting to a reference unit and converting from the reference unit. This avoids writing a quadratic number of conversion functions.

No-op conversions convert to the same unit, and distributing units in a vector is similar.

Conversions between multiplicative units with the same dimensions for their base units is simply finding the conversion factor. If the dimensions don't match we look for substance conversions. Substance conversions for related units technically need a temperature and pressure, but usually it's "1 atm, room temperature", and you can create new substances like "water at 20 C" or redefine the value if you need to. These parameters should be part of the unit database definition.

If the multiplicative unit consists solely of a dimensioned logarithmic unit, we allow conversion to the related linear unit, e.g. ``0 dBm = 1 mW``. Similarly dimensionless logarithmic units by themselves convert to pure numbers.

Conversions for affine units are simple applications of the definitions.

Percentages convert to pure numbers.

All conversions are bidirectional and can be chained, so we get equivalence classes of quantities.

Convention contexts
===================

Conventions are "natural" equations obtained by setting a constant to a dimensionless 1. Unlike conversions, they relax the dimensional analysis, as opposed to conforming to it. For example, setting the speed of light to 1, one obtains the convention ``1 s = 299792458 m``. Normal dimensional analysis would consider this an ill-formed equation due to the different dimensions. But it means we multiply or divide by the constant as appropriate, e.g. ``1`` can be converted to ``299792458 m / s``.  The procedure is to cancel the units as much as possible and then add them back at the end.

Conventions can disagree, e.g. instead of the speed of light we could set gravity to 1 giving ``1 s = 9.8 m``. If we try to use both the speed of light and gravity in the same context the dimensional analysis collapses with ``9.8 = 299792458``.

To prevent ambiguity conventions are limited to a context (block or formula) and the non-simplified units of the inputs and outputs must be known. So you write:

::

  wavelength = 1*meter

  withConvention (speed_of_light == 1) {
    frequency = 1/wavelength in Hz
  }

  # or

  frequency = withConvention (speed_of_light == 1) (1/wavelength) in Hz

and the calculation is ``frequency = 1/wavelength * speed_of_light``.

One useful convention is ``ignore_units``, where all units are set equal to 1 and the relationships between units are ignored.

Arithmetic
==========

Summing quantities with the same derived units or substance units is simple, just add the numbers. E.g. ``1 N + 2 N = 3 N``. If the derived and substance units differ, we use a promotion rule to determine a common unit - the first argument's unit, or the most specific unit, or whatever. Then we convert both arguments to that unit and perform the operation. If there is a surrounding ``convert`` we may be able to optimize the computation to use the desired output unit as the common unit, but conceptually the promotion rule still is applied. This works for all arithmetic operations.

For logarithmic units adding/subtracting two of the same unit is fine. Adding or subtracting a dimensionless logarithmic unit to a non-logarithmic unit multiplies by ``b^(x/f)``. Adding/subtracting two distinct dimensionless logarithmic units uses promotion rules to pick a common dimensionless logarithmic unit. Dimensioned logarithmic units are similar but the addition/subtraction only works if the non-logarithmic units match. Adding dimensionless logarithmic units to dimensioned logarithmic units promotes the dimensionless unit to match the dimensioned. Multiplying or dividing a logarithmic unit by a pure number acts on the number; quantities that aren't pure numbers generate an error.

Percentage units are similar to dimensionless logarithmic units, adding/subtracting is ``a + x% = a * (1 + x/100)``/ ``a - x% = a * (1 - x/100)``. Multiplying percents multiplies their pure value, with other units and with itself ``a%*b%=(a*b/100)%``.

For affine units, the only operations are those of an affine space: subtracting two affine quantities to get a vector difference, and adding/subtracting a vector from an affine quantity, like ``1 degC + 3 kelvin = 4 degC``. Multiplication and division aren't defined.

@logscale(symb,abbr,name,base,prefactor,irp)

Define a logarithmic scale identified by symbol symb. dB_rp and dB_p, indicate root-power ratios or power ratios.

prefactor is the prefactor out in front of the logarithm for this log scale.

Measurement of civil time is dependent on location (time zone). This applies even to "time modulo 12 or 24 hours", because of DST and day start.

Angles:
* Compass headings are an affine space, e.g. North + North makes no sense.
* Circular angles are rotational angles mod 2*pi. Mainly useful for compressing the representation, and confusing people by writing ``400 degrees == 40 degrees``.

Torque and work (energy) are both force times length, but not equivalent. Torque is a 2-vector (exterior product) while energy is a scalar (dot or inner product).

Fuel consumption in gallons per mile is a volume divided by a length, yielding Length^2. But adding fuel consumption to an area makes no sense. The productivity of a lumber mill in feet per day is not the same concept as speed, even though both have dimensions Length/Time. The problem is clearer if we add types of material to units. Given the density of salt, computing the sum of a cup of salt and a pound of salt in cups of salt is possible. But dimensional analysis requires an explicit conversion factor. Similarly concentrations of salt in water are Volume Salt/Volume Water and Mass Salt/Volume Water and these should be convertible. Without types of material, Volume/Volume is dimensionless, and we could compare it to slope, Length/Length, or clock error rate, Time/Time.

Modes
=====

When working on a program with units I devised several modes of operation for units:
* Unchecked: conversion by explicit multiplication, unit annotations in comments
* Automatic unit conversion: unit annotations in code, conversions specified in a comprehensive list
* Semi-automatic unit conversion: unit annotations in code, conversions specified contextually
* Manual unit conversions: unit annotations in code, units only go through basic 1-1 normalization. All non 1-1 conversions must be done by multiplying by a conversion factor annotated with units (e.g. 1000 g/kg) or a conversion function. Can assert that a variable has specific units.
* Raw: maximum speed. unit annotations are ignored or replaced with multiplication by 1, conversion factors are used directly.

Generally, you start with unchecked code. Then you add unit annotations - you haven't determined a consistent set of units, so automatic conversion is necessary. Then you can standardize the units and conversions to a manual system. Then turn on/off checking to go back and forth to a raw system. Raw is still not unchecked, because the annotations are in the code.

Static compilation or JITting optimizations such as inlining should be able to optimize automatic unit conversion to the speed of raw, making manual conversion unnecessary as a coding style. But I found manual conversion to be quite a speedup when working with interpreted (Python) code. Checking/converting units on every operation is slow if it's interpreted. The problem I ran into was integrating a function - constructing and converting the units on every sample was too slow.
