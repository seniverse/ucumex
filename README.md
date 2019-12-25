# Ucumex

A elixir lib according to [THE UNIFIED CODE FOR UNITS OF MEASURE](http://unitsofmeasure.org/ucum.html) to convert unit.

## Note!

- **Not Support Comment**
- **Special Units are not Support Except Fahrenheit Degree and Celsius degree**

## Examples

```elixir
{:ok, op} = Ucumex.convertor("km", "m")

op.(3) # -> 3.0e3
```

```elixir
# "[degRe]" is special unit
Ucumex.convertor("km", "[degRe]")

** (NotImplementedError) handler is not implemented for this special unit.
    (ucumex) lib/ucumex.ex:390: Ucumex.to_basic_units/1
    (ucumex) lib/ucumex.ex:48: Ucumex.to_bin_op/2
    (ucumex) lib/ucumex.ex:24: Ucumex.unit_to_std/1
    (ucumex) lib/ucumex.ex:17: Ucumex.convertor/2
```

```elixir
{:error, :unit_not_match} = Ucumex.convertor("km", "L")
```
