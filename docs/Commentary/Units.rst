Units
#####

Fortress, Julia, and Python all have unit libraries. So it seems worthwhile to implement these. Although quantities with units will probably never be the default values used by APIs, units are useful for safety.

Quantities
==========

A quantity is a value represented by a number and a unit, ``Quantity num unit``. For example "10 avoirdupois pounds", "10 lbs", "160 ounces" are all (distinct) quantities. The syntax is juxtaposition like ``1 kg`` or an infix "unit" operator ``2 unit (kg^2)`` for more complicated expressions. A pure number is a dimensionless quantity ``x unit 1``, convertible to the raw numeric type.

The number is usually a real number, but a vector or a complex number can be combined with a unit as well. A vector quantity is distinct from a vector of scalar quantities all with the same unit (but is interconvertible, discussed next) You can also have a vector of quantities with different units, e.g. ``(1 m, 2 s)``. Some units may technically not be defined for negative numbers, e.g. mass, but in practice using real numbers makes computation much more convenient. Also, complex number must have split units, as when you take the log of a pure complex quantities you get a real part in nepers and an imaginary part in radians.

The number must be represented in some format, so quantities are further divided according to their number's format, e.g. a ``(10:float32) meter`` is distinct from a ``(10:float64) meter``. You can have special formats, e.g. binary radians are a format ``{0 .. 2^(n−1)}*2*pi/2^n`` combined with the radian unit. As with all formats there are restrictions on the range and precision of numbers that may be represented.

An expression like "10 pounds" may be interpreted as multiple quantities, e.g. "10 avoirdupois pounds" (a weight) or "10 British pounds" (a currency), depending on context. The context could be represented via an implicit argument or whatever, but it seems a lot easier to forbid ambiguity and devise a coherent set of unit names, e.g. Frink's "pound is weight and GBP is currency". So expressions are required to be unambiguous.

Units
=====

The units we have are multiplicative units, affine units, logarithmic units, and percentages.

Basic units
-----------

These are the basic units you'll find in any system:
* The 7 SI base units, like second, meter, kilogram
* Derived units, like feet, kilometers, newtons, etc., and their SI prefixes

In addition we have the information unit nat, and counting units like bits, molecules, neutrons, atoms, cycles, etc., all of which are treated simply as base dimensions.

Angles
------

Radians are better off as a base unit, because then degrees can be converted naturally to radians, instead of being shoehorned in as a multiplicative factor. See :cite:`quinceyImplicationsAdoptingPlane2016` or https://arxiv.org/pdf/2203.12392.pdf for how formulas adjust to accommodate angles as a dimension with an explicit dimensional constant. For example torque and work (energy) in a conventional setting are both force times length and have the same type and unit, but in fact they are not equivalent in two ways:
* In a geometric algebra setting, torque is a 2-vector (exterior product) while energy is a scalar (dot or inner product). So the numeric types are different.
* The proper unit for torque is ``N*m/rad``, rather than ``N*m``. :cite:`quinceyReplyCommentAngles2022`

To return to SI you can do ``use radian_convention { }``, which sets ``1 rad = 1`` in the block. Conventions are discussed more later on.

As far as types of angles, there are a lot:
* Rotational angles, normal numeric type plus an angle unit
* Classification of angles in [0,360]:

  * Zero: {0}
  * Acute: (0,90) degrees
  * Right: {90}
  * Obtuse: (90,180)
  * Straight: {180}
  * Reflex: (180,360)
  * Complete: {360}

  * Skew: acute or obtuse
  * Oblique: skew or reflex except 270
  * Quadrantal: {0,90,180,270}

  * Reference angle: zero, acute, or right
  * Ordinary angle: reference or obtuse
  * Convex: ordinary or straight
  * "Circular" angle: convex or reflex (all but complete), i.e. [0,360) degrees

* "Measurable" angle: angle in (-360,360). Relative to base, positive is counterclockwise, negative is clockwise.
* "Circular" angles are rotational angles mod 2*pi. They have an angle unit but a modulo numeric type. Mainly useful for compressing the representation, and confusing people by writing ``400 degrees == 40 degrees``.
* Compass bearings are an affine space over circular angles, e.g. North + North makes no sense, but North - (North+1 degree) is -1 degree. The magnitude of the difference is limited to (-180,180) degrees corresponding to the mod 2 pi. Different unit because it's affine.
* "Principal sine" angle: [-90,90] degrees. Can be represented by its sine.
* "Principal cosine" angle: convex angle, [0,180] degrees. Can be represented by its cosine.
* "Principal tangent" angle: angle of (-90,90) degrees. Can be represented by its tangent.
* "Principal cotangent" angle: angle of (0,180) degrees. Can be represented by its cotangent.
* "Principal secant" angle: [0,180]\\{90} degrees. Can be represented by its secant.
* "Principal cosecant" angle: [-90,90]\\{0} degrees. Can be represented by its cosecant.

Substance units
---------------

Substance units are a unit plus a substance name, e.g. ``g NaCl`` or ``g salt``. To motivate these, consider fuel consumption: in gallons per mile, it is a volume divided by a length, yielding base dimensions of Length^2. But adding fuel consumption to an area makes no sense. If the volume is a substance unit, then the length has no relation and we cannot reduce ``L fuel / m``, as is proper. Similarly the productivity of a lumber mill ``feet timber / day`` is not the same concept as speed ``feet / day``, even though both have dimensions Length/Time. The calculations become clearer if we add types of material to units.

Substance units also add power. Given the density of salt, a cup of salt is equivalent to some number of pounds of salt, and can be converted automatically. Similarly concentrations of salt in water can be given as V/V (e.g. mL Salt/L Water), W/V (g Salt/L Water), or W/W (g Salt/g Water) and with substance units these are automatically convertible. Without substance units, V/V would be dimensionless and would erroneously convert 1-1 to W/W.

Barry N. Taylor of NIST has declared substance units verboten (`link <https://www.nist.gov/pml/special-publication-811/nist-guide-si-chapter-7-rules-and-style-conventions-expressing-values>`__, section 7.5) but he gives no justification and other scientists have complained about the overly strict conventions so it seems he is just another grammar stickler. Research shows `resistance to grammar change is futile <https://www.theguardian.com/science/2017/nov/01/resistance-to-changes-in-grammar-is-futile-say-researchers>`__ so it's better to include substance units from the beginning - if you don't like them don't use them.

Affine units
------------

So far all the units have been multiplicative units, so that a product of powers of units is a new unit. Essentially, the unit is a variable, e.g. when we say a mass is 10 grams, this means m = 10 * gram where "gram" is a variable.

Then there are affine units, which are a scale and offset referenced to a derived multiplicative unit. Examples are Celsius referenced to Kelvin, Fahrenheit referenced to Rankine, timescales to second, dates to day, locations to 3D vector of kilometers. A GCS coordinate such as ``(17.8416656,-124.0563834) WGS 84`` describes the location on the surface of the ellipsoid, which can be subtracted from another point such as "Null Island" at 0,0 or the center of the earth to obtain a 3D vector.

A unit like ``degF / kg`` as used in old scientific work doesn't mean the affine Fahrenheit scale, instead ``degF`` is interpreted as a temperature difference. This could be hacked in, but similar to pounds weight vs. pounds currency, it is easier to make it unambiguous by requiring ``degR / kg`` in this context and forbidding combining affine units with other units. Rankine has the same difference size as Fahrenheit and since it's referenced to absolute zero it scales properly.

Logarithmic units
-----------------

There are three kinds that seem worth exploring:

* Dimensioned logarithmic units, ``f * log_b(x/r)``, where factor f, reference quantity r (with unit), and log base b are part of the unit. For example dBm.
* Dimensionless logarithmic units, ``f * log_b(q)``, similar but ``q`` is just a unitless quantity. For example dB.
* Molyneux's logarithmic units, ``x unitA + unitA log_b unitB``, written ``x unit (unitA +* logb unitB)`` or ``x unit (logb unitB)`` when ``unitA`` is 1. These result from taking the log of a quantity with units a power of ``unitB``. For example pH is unit ``log10 (mol^(-1)*dm^3)``.

Percentages
-----------

Percentage seems like it should be a unit, similar to dimensionless logarithmic units but less well-behaved. Hence we forbid combining percentage units with other units.

Parts per notation
------------------

As far as parts per million (ppm), ppb, ppt, etc., Wikipedia has a `handy conversion table <https://en.wikipedia.org/wiki/Parts-per_notation#SI-compliant_expressions>`__ to SI notation, and the SI notation seems much clearer. Furthermore with substance units the SI-like ratio doesn't collapse to a dimensionless value, whereas ppm is ambiguous. So writing it out seems better.

Subdivision quantities
======================

These are new quantity types, equivalent to number + base unit, but expressed with multiple numbers representing subdivisions. For example DMS notation 1 degree 1 minute 1 second, equal to 1 + 1/60 + 1/360 degrees, or 6 feet 2 inches, equal to 74 inches.

Accessors
=========

Stripping a quantity gives the bare number, e.g. ``strip (2 mm) = 2``. It's safer to write the unit you expect to be stripped, ``strip x mm``, which converts the unit if it doesn't match and fails if unconvertible. This together with the ``Quantity x unit`` constructor allows interoperating with non-unit-aware APIs. The unit accessor ``unit (2 mm) = mm`` allows writing higher-order functions on quantities. Recursive stripping removes units in vectors, ``rstrip (2 mm, 3 kg) = (2,3)``.

There is also a ``strip_substance`` function, similar to strip but used for removing the substance part of the unit, like collapsing ``mol A / mol B`` to ``mol/mol = 1``.

Conversions
===========

We want to convert quantities to other units, but the desired result format should also be specified, since the type may contain embedded dimensional units. So the main API is the basic ``convert quantity type``. For example distributing units in a vector is ``convert ((1,2,3) m) [Real unit meter]  = (1 m, 2 m, 3 m)``. And of course there are the basics like ``convert (1 km) (Real unit feet) = 3280.8399 feet``. Maybe there is also an automated unit guesser based on dimensional analysis so we can do ``convert (1 foot) (Real unit GuessSI) = 0.3048 m``.

The conversion should use the most precise calculation possible, passing the formula through a floating-point accuracy tool like Herbie. In practice it seems libraries implement conversions by converting to a reference unit and converting from the reference unit. This avoids writing a quadratic number of conversion functions.

No-op conversions convert to the same unit, and distributing units in a vector is similar.

Conversions between multiplicative units with the same dimensions for their base units is simply finding the conversion factor. If the dimensions don't match we look for substance conversions. Substance conversions for related units technically need a temperature and pressure, but usually it's "1 atm, room temperature", and you can create new substances like "water at 20 C" or redefine the value if you need to. These parameters should be part of the unit database definition.

If the multiplicative unit consists solely of a dimensioned logarithmic unit, we allow conversion to the related linear unit, e.g. ``0 dBm = 1 mW``. Similarly dimensionless logarithmic units by themselves convert to pure numbers.

Conversions for affine units are simple applications of the definitions.

Percentages convert to pure numbers.

All conversions are bidirectional and can be chained, so we get equivalence classes of quantities.

There is syntax sugar for conversion. A unit applied to a quantity (as opposed to a numeric type) converts the quantity but preserves the numeric type. For example ``(1 foot) meter`` is ``convert (1 foot) (Real unit meter)``. The numeric type is guessed via a function.

Convention contexts
===================

Conventions are "natural" equations obtained by setting a constant to a dimensionless 1. They relax the dimensional analysis by adding new conversions. For example, setting the speed of light to 1, one obtains the convention ``1 s = 299792458 m``. Normal dimensional analysis would consider this an ill-formed equation due to the different dimensions. But it means we multiply or divide by the constant as appropriate, e.g. ``1`` can be converted to ``299792458 m / s``.  The procedure is to cancel the units as much as possible and then add them back at the end.

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

One useful convention is ``ignore_units``, where all units are set equal to 1 and the relationships between units are ignored, you can use it like ``use ignore_units { q unit km }`` which is equivalent to ``Quantity (strip q) km``.

Arithmetic
==========

Summing or subtracting quantities with the same derived units or substance units is simple, just add the numbers. E.g. ``1 N + 2 N = 3 N``. If the derived and substance units differ, we use a promotion rule to determine a common unit - the first argument's unit, or the most specific unit, or whatever. Then we convert both arguments to that unit and perform the operation. If there is a surrounding ``convert`` we may be able to optimize the computation to use the desired output unit as the common unit, but conceptually the promotion rule still is applied.

For multiplication and division, a different promotion rule identifies the most simplified form of ``unitsA * unitsB`` or ``unitsA unitsB^-1``.

For logarithmic units adding/subtracting two of the same unit is fine. Adding or subtracting a dimensionless logarithmic unit to a non-logarithmic unit multiplies by ``b^(x/f)``. Adding/subtracting two distinct dimensionless logarithmic units uses promotion rules to pick a common dimensionless logarithmic unit. Dimensioned logarithmic units are similar but the addition/subtraction only works if the non-logarithmic units match. Adding dimensionless logarithmic units to dimensioned logarithmic units promotes the dimensionless unit to match the dimensioned. Multiplying or dividing a logarithmic unit by a pure number acts on the number; quantities that aren't pure numbers generate an error.

Percentage units are similar to dimensionless logarithmic units, adding/subtracting is ``a + x% = a * (1 + x/100)``/ ``a - x% = a * (1 - x/100)``. Multiplying percents multiplies their pure value, with other units and with itself ``a%*b%=(a*b/100)%``.

For affine units, the only operations are those of an affine space: subtracting two affine quantities to get a vector difference, and adding/subtracting a vector from an affine quantity, like ``1 degC + 3 kelvin = 4 degC``. Multiplication and division aren't defined.

and other dimensionless operations is an issue.

Molyneux's approach can be extended to handling exponents, but raising a unit to a dimensionless power is the main operation and doesn't need another type of unit.

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
