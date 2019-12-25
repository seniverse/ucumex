defmodule UcumexTest do
  use ExUnit.Case
  doctest Ucumex

  require Logger

  test "test mL" do
    unit = "mL"
    {v, u} = Ucumex.unit_to_std(unit)

    assert v * 1.0e6 - 1.0 < 0.001
    assert u == [{"m", 3}]
  end

  test "test L" do
    unit = "L"
    {v, u} = Ucumex.unit_to_std(unit)

    assert v * 1.0e3 - 1.0 < 0.001
    assert u == [{"m", 3}]
  end

  test "test ng/(8.h)" do
    unit = "ng/(8.h)"
    {v, u} = Ucumex.unit_to_std(unit)

    assert v - (8 * 3600 * 1.0e-9) < 0.001
    assert u == [{"g", 1}, {"s", -1}]
  end

  test "test mmol/(8.h.kg)" do
    unit = "mmol/(8.h.kg)"
    {v, u} = Ucumex.unit_to_std(unit)

    assert v - (6.0221367 * 1.0e23 * 1.0e-3 / (8 * 3600 * 1000)) < 0.001
    assert u == [{"10*", 23}, {"s", -1}, {"g", -1}]
  end

  test "test mL/cm[H2O]" do
    unit = "mL/cm[H2O]"
    {v, u} = Ucumex.unit_to_std(unit)

    assert v - 1.0197162129779283e-11 < 0.001
    assert u == [{"s", 2}, {"g", -1}, {"m", 4}]
  end

  test "test ug/m2" do
    unit = "ug/m2"
    {v, u} = Ucumex.unit_to_std(unit)

    assert v - 1.0e-6 < 0.001
    assert u == [{"g", 1}, {"m", -2}]
  end

  test "kCel2/s" do
    unit = "kCel2/s"
    {v, u} = Ucumex.unit_to_std(unit)

    assert v - (1000 * 1000 + 273.15) < 0.001
    assert u == [{"K", 2}, {"s", -1}]
  end

  test "mile" do
    unit = "[mi_i]"
    {v, u} = Ucumex.unit_to_std(unit)

    assert v - (1609.344) < 0.001
    assert u == [{"m", 1}]
  end

  test "[in_i'Hg]" do
    unit = "[in_i'Hg]"
    {v, _u} = Ucumex.unit_to_std(unit)

    assert v - 3386378.8 < 0.001
  end

  test "f -> c" do
    {:ok, op} = Ucumex.convertor("[degF]", "Cel")
    assert op.(1) - -17.22 < 0.001
  end

  test "c -> f" do
    {:ok, op} = Ucumex.convertor("Cel", "[degF]")
    assert op.(1) - 33.8 < 0.001
  end
end

# m|s|g|(rad)|K|C|(cd)|
