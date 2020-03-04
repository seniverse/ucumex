defmodule Ucumex do
  @moduledoc """
  Documentation for Ucumex.
  """

  require Logger

  @spec convertor(String.t, String.t) :: {:error, :unit_not_match} | {:ok, (number -> float)}
  def convertor("[degF]", "Cel") do
    {:ok, fn v -> (v - 32) / 1.8 end}
  end
  def convertor("Cel", "[degF]") do
    {:ok, fn v -> v * 1.8 + 32 end}
  end
  def convertor("Cel", "K") do
    {:ok, fn v -> v + 273.15 end}
  end
  def convertor("K", "Cel") do
    {:ok, fn v -> v - 273.15 end}
  end
  def convertor("[degF]", "K") do
    {:ok, fn v -> (v - 32) * 5 / 9 + 273.15 end}
  end
  def convertor("K", "[degF]") do
    {:ok, fn v -> (v - 273.15) * 9 / 5 + 32 end}
  end
  def convertor(from, to) do
    {factor_from, unit_from} = unit_to_std(from)
    {factor_to,   unit_to}   = unit_to_std(to)

    case validate_unit(unit_from, unit_to) do
      :ok ->
        {:ok, fn v -> v * factor_from / factor_to end}
      :error ->
        {:error, :unit_not_match}
    end
  end

  @spec unit_to_std(String.t) :: {float, [{String.t, integer}]}
  def unit_to_std(unit) do
    unit
    |> to_bin_op
    |> to_std
    |> check_low_op
  end

  defp validate_unit([], []) do
    :ok
  end
  defp validate_unit([f_h | f_r], [t_h | t_r]) when f_h == t_h do
    validate_unit(f_r, t_r)
  end
  defp validate_unit(_, _) do
    :error
  end

  defp to_bin_op({op, term_a, term_b}, _) when is_tuple(term_a) and is_tuple(term_b) do
    {op, to_bin_op(term_a, ""), to_bin_op(term_b, "")}
  end
  defp to_bin_op({_op, 1, 1}, prev_unit) do
    prev_unit
  end
  defp to_bin_op({_op, 1, "1"}, prev_unit) when is_tuple(prev_unit) do
    prev_unit
  end
  defp to_bin_op({_op, 1, "1"}, prev_unit) when is_number(prev_unit) do
    prev_unit
  end
  defp to_bin_op({op, factor, "1"}, prev_unit) do
    {op, factor, prev_unit |> do_bin_op |> to_bin_op(prev_unit)}
  end
  defp to_bin_op({op, factor, unit}, prev_unit) when unit == prev_unit do
    {op, factor, unit}
  end
  defp to_bin_op({op, factor, unit}, _prev_unit) do
    {op, factor, unit |> to_basic_units |> to_bin_op(unit)}
  end

  defp to_bin_op(string) when is_binary(string) do
    string |> String.to_charlist |> to_bin_op
  end
  defp to_bin_op(chars) when is_list(chars) do
    chars |> do_bin_op |> to_bin_op("")
  end

  defp do_bin_op(s) when is_binary(s) do
    s |> String.to_charlist |> do_bin_op
  end
  defp do_bin_op(chars) when is_list(chars) do
    {:ok, tokens, _} = :ucumex_lexer.string(chars)
    {:ok, bin_op} = :ucumex_parser.parse(tokens)
    bin_op
  end

  defp to_std({op, {_, _, _}=term_a, {_, _, _}=term_b}) do
    op.(to_std(term_a), to_std(term_b))
  end
  defp to_std({op, factor, unit}) when is_tuple(unit) do
    op.(factor, to_std(unit))
  end
  defp to_std({op, factor, unit}) do
    op.(factor, unit)
  end

  defp check_low_op({low_op, u, v}) do
    low_op.({u, v})
  end
  defp check_low_op(result) do
    result
  end

  defp mulp(factor, {v, unit}) do
    {factor * v, unit}
  end
  defp mulp(factor, v) do
    factor * v
  end

  defp cel(273.15, {val, unit}) do
    {fn {v, u} -> {v + 273.15, u} end, val, unit}
  end
  defp cel(factor, {low_op, val, unit}) do
    {low_op, factor * val, unit}
  end

  defp deg_f(459.67, {val, unit}) do
    {fn {v, u} -> {5 / 9 * (v + 459.67), u} end, val, unit}
  end
  defp deg_f(factor, {low_op, val, unit}) do
    {low_op, factor * val, unit}
  end

  defp to_basic_units("10*"), do: {&mulp/2, 10, "1"}
  defp to_basic_units("10^"), do: {&mulp/2, 10, "1"}
  defp to_basic_units("[pi]"), do: {&mulp/2, 3.1415926535897932384626433832795028841971693993751058209749445923, "1"}
  defp to_basic_units("%"), do: {&mulp/2, 1, "10*-2"}
  defp to_basic_units("[ppth]"), do: {&mulp/2, 1, "10*-3"}
  defp to_basic_units("[ppm]"), do: {&mulp/2, 1, "10*-6"}
  defp to_basic_units("[ppb]"), do: {&mulp/2, 1, "10*-9"}
  defp to_basic_units("[pptr]"), do: {&mulp/2, 1, "10*-12"}
  defp to_basic_units("mol"), do: {&mulp/2, 6.0221367, "10*23"}
  defp to_basic_units("sr"), do: {&mulp/2, 1, "rad2"}
  defp to_basic_units("Hz"), do: {&mulp/2, 1, "s-1"}
  defp to_basic_units("N"), do: {&mulp/2, 1, "kg.m/s2"}
  defp to_basic_units("Pa"), do: {&mulp/2, 1, "N/m2"}
  defp to_basic_units("J"), do: {&mulp/2, 1, "N.m"}
  defp to_basic_units("W"), do: {&mulp/2, 1, "J/s"}
  defp to_basic_units("A"), do: {&mulp/2, 1, "C/s"}
  defp to_basic_units("V"), do: {&mulp/2, 1, "J/C"}
  defp to_basic_units("F"), do: {&mulp/2, 1, "C/V"}
  defp to_basic_units("Ohm"), do: {&mulp/2, 1, "V/A"}
  defp to_basic_units("S"), do: {&mulp/2, 1, "Ohm-1"}
  defp to_basic_units("Wb"), do: {&mulp/2, 1, "V.s"}
  defp to_basic_units("T"), do: {&mulp/2, 1, "Wb/m2"}
  defp to_basic_units("H"), do: {&mulp/2, 1, "Wb/A"}
  defp to_basic_units("lm"), do: {&mulp/2, 1, "cd.sr"}
  defp to_basic_units("lx"), do: {&mulp/2, 1, "lm/m2"}
  defp to_basic_units("Bq"), do: {&mulp/2, 1, "s-1"}
  defp to_basic_units("Gy"), do: {&mulp/2, 1, "J/kg"}
  defp to_basic_units("Sv"), do: {&mulp/2, 1, "J/kg"}
  defp to_basic_units("deg"), do: {&mulp/2, 2, "[pi].rad/360"}
  defp to_basic_units("gon"), do: {&mulp/2, 0.9, "deg"}
  defp to_basic_units("'"), do: {&mulp/2, 1, "deg/60"}
  defp to_basic_units("''"), do: {&mulp/2, 1, "'/60"}
  defp to_basic_units("l"), do: {&mulp/2, 1, "dm3"}
  defp to_basic_units("L"), do: {&mulp/2, 1, "l"}
  defp to_basic_units("ar"), do: {&mulp/2, 100, "m2"}
  defp to_basic_units("min"), do: {&mulp/2, 60, "s"}
  defp to_basic_units("h"), do: {&mulp/2, 60, "min"}
  defp to_basic_units("d"), do: {&mulp/2, 24, "h"}
  defp to_basic_units("a_t"), do: {&mulp/2, 365.24219, "d"}
  defp to_basic_units("a_j"), do: {&mulp/2, 365.25, "d"}
  defp to_basic_units("a_g"), do: {&mulp/2, 365.2425, "d"}
  defp to_basic_units("a"), do: {&mulp/2, 1, "a_j"}
  defp to_basic_units("wk"), do: {&mulp/2, 7, "d"}
  defp to_basic_units("mo_s"), do: {&mulp/2, 29.53059, "d"}
  defp to_basic_units("mo_j"), do: {&mulp/2, 1, "a_j/12"}
  defp to_basic_units("mo_g"), do: {&mulp/2, 1, "a_g/12"}
  defp to_basic_units("mo"), do: {&mulp/2, 1, "mo_j"}
  defp to_basic_units("t"), do: {&mulp/2, 1.0e3, "kg"}
  defp to_basic_units("bar"), do: {&mulp/2, 1.0e5, "Pa"}
  defp to_basic_units("u"), do: {&mulp/2, 1.6605402e-24, "g"}
  defp to_basic_units("AU"), do: {&mulp/2, 149597.870691, "Mm"}
  defp to_basic_units("pc"), do: {&mulp/2, 3.085678e16, "m"}
  defp to_basic_units("[c]"), do: {&mulp/2, 299792458, "m/s"}
  defp to_basic_units("[h]"), do: {&mulp/2, 6.6260755e-34, "J.s"}
  defp to_basic_units("[k]"), do: {&mulp/2, 1.380658e-23, "J/K"}
  defp to_basic_units("[eps_0]"), do: {&mulp/2, 8.854187817e-12, "F/m"}
  defp to_basic_units("[mu_0]"), do: {&mulp/2, 1, "4.[pi].10*-7.N/A2"}
  defp to_basic_units("[e]"), do: {&mulp/2, 1.60217733e-19, "C"}
  defp to_basic_units("eV"), do: {&mulp/2, 1, "[e].V"}
  defp to_basic_units("[m_e]"), do: {&mulp/2, 9.1093897e-28, "g"}
  defp to_basic_units("[m_p]"), do: {&mulp/2, 1.6726231e-24, "g"}
  defp to_basic_units("[G]"), do: {&mulp/2, 6.67259e-11, "m3.kg-1.s-2"}
  defp to_basic_units("[g]"), do: {&mulp/2, 980665.0e-5, "m/s2"}
  defp to_basic_units("Torr"), do: {&mulp/2, 133.322, "Pa"}
  defp to_basic_units("atm"), do: {&mulp/2, 101325, "Pa"}
  defp to_basic_units("[ly]"), do: {&mulp/2, 1, "[c].a_j"}
  defp to_basic_units("gf"), do: {&mulp/2, 1, "g.[g]"}
  defp to_basic_units("Ky"), do: {&mulp/2, 1, "cm-1"}
  defp to_basic_units("Gal"), do: {&mulp/2, 1, "cm/s2"}
  defp to_basic_units("dyn"), do: {&mulp/2, 1, "g.cm/s2"}
  defp to_basic_units("erg"), do: {&mulp/2, 1, "dyn.cm"}
  defp to_basic_units("P"), do: {&mulp/2, 1, "dyn.s/cm2"}
  defp to_basic_units("Bi"), do: {&mulp/2, 10, "A"}
  defp to_basic_units("St"), do: {&mulp/2, 1, "cm2/s"}
  defp to_basic_units("Mx"), do: {&mulp/2, 1.0e-8, "Wb"}
  defp to_basic_units("G"), do: {&mulp/2, 1.0e-4, "T"}
  defp to_basic_units("Oe"), do: {&mulp/2, 250, "/[pi].A/m"}
  defp to_basic_units("Gb"), do: {&mulp/2, 1, "Oe.cm"}
  defp to_basic_units("sb"), do: {&mulp/2, 1, "cd/cm2"}
  defp to_basic_units("Lmb"), do: {&mulp/2, 1, "cd/cm2/[pi]"}
  defp to_basic_units("ph"), do: {&mulp/2, 1.0e-4, "lx"}
  defp to_basic_units("Ci"), do: {&mulp/2, 37.0e9, "Bq"}
  defp to_basic_units("R"), do: {&mulp/2, 2.58e-4, "C/kg"}
  defp to_basic_units("RAD"), do: {&mulp/2, 100, "erg/g"}
  defp to_basic_units("REM"), do: {&mulp/2, 1, "RAD"}
  defp to_basic_units("[in_i]"), do: {&mulp/2, 254.0e-2, "cm"}
  defp to_basic_units("[ft_i]"), do: {&mulp/2, 12, "[in_i]"}
  defp to_basic_units("[yd_i]"), do: {&mulp/2, 3, "[ft_i]"}
  defp to_basic_units("[mi_i]"), do: {&mulp/2, 5280, "[ft_i]"}
  defp to_basic_units("[fth_i]"), do: {&mulp/2, 6, "[ft_i]"}
  defp to_basic_units("[nmi_i]"), do: {&mulp/2, 1852, "m"}
  defp to_basic_units("[kn_i]"), do: {&mulp/2, 1, "[nmi_i]/h"}
  defp to_basic_units("[sin_i]"), do: {&mulp/2, 1, "[in_i]2"}
  defp to_basic_units("[sft_i]"), do: {&mulp/2, 1, "[ft_i]2"}
  defp to_basic_units("[syd_i]"), do: {&mulp/2, 1, "[yd_i]2"}
  defp to_basic_units("[cin_i]"), do: {&mulp/2, 1, "[in_i]3"}
  defp to_basic_units("[cft_i]"), do: {&mulp/2, 1, "[ft_i]3"}
  defp to_basic_units("[cyd_i]"), do: {&mulp/2, 1, "[yd_i]3"}
  defp to_basic_units("[bf_i]"), do: {&mulp/2, 144, "[in_i]3"}
  defp to_basic_units("[cr_i]"), do: {&mulp/2, 128, "[ft_i]3"}
  defp to_basic_units("[mil_i]"), do: {&mulp/2, 1.0e-3, "[in_i]"}
  defp to_basic_units("[cml_i]"), do: {&mulp/2, 1, "[pi]/4.[mil_i]2"}
  defp to_basic_units("[hd_i]"), do: {&mulp/2, 4, "[in_i]"}
  defp to_basic_units("[ft_us]"), do: {&mulp/2, 1200, "m/3937"}
  defp to_basic_units("[yd_us]"), do: {&mulp/2, 3, "[ft_us]"}
  defp to_basic_units("[in_us]"), do: {&mulp/2, 1, "[ft_us]/12"}
  defp to_basic_units("[rd_us]"), do: {&mulp/2, 16.5, "[ft_us]"}
  defp to_basic_units("[ch_us]"), do: {&mulp/2, 4, "[rd_us]"}
  defp to_basic_units("[lk_us]"), do: {&mulp/2, 1, "[ch_us]/100"}
  defp to_basic_units("[rch_us]"), do: {&mulp/2, 100, "[ft_us]"}
  defp to_basic_units("[rlk_us]"), do: {&mulp/2, 1, "[rch_us]/100"}
  defp to_basic_units("[fth_us]"), do: {&mulp/2, 6, "[ft_us]"}
  defp to_basic_units("[fur_us]"), do: {&mulp/2, 40, "[rd_us]"}
  defp to_basic_units("[mi_us]"), do: {&mulp/2, 8, "[fur_us]"}
  defp to_basic_units("[acr_us]"), do: {&mulp/2, 160, "[rd_us]2"}
  defp to_basic_units("[srd_us]"), do: {&mulp/2, 1, "[rd_us]2"}
  defp to_basic_units("[smi_us]"), do: {&mulp/2, 1, "[mi_us]2"}
  defp to_basic_units("[sct]"), do: {&mulp/2, 1, "[mi_us]2"}
  defp to_basic_units("[twp]"), do: {&mulp/2, 36, "[sct]"}
  defp to_basic_units("[mil_us]"), do: {&mulp/2, 1.0e-3, "[in_us]"}
  defp to_basic_units("[in_br]"), do: {&mulp/2, 2.539998, "cm"}
  defp to_basic_units("[ft_br]"), do: {&mulp/2, 12, "[in_br]"}
  defp to_basic_units("[rd_br]"), do: {&mulp/2, 16.5, "[ft_br]"}
  defp to_basic_units("[ch_br]"), do: {&mulp/2, 4, "[rd_br]"}
  defp to_basic_units("[lk_br]"), do: {&mulp/2, 1, "[ch_br]/100"}
  defp to_basic_units("[fth_br]"), do: {&mulp/2, 6, "[ft_br]"}
  defp to_basic_units("[pc_br]"), do: {&mulp/2, 2.5, "[ft_br]"}
  defp to_basic_units("[yd_br]"), do: {&mulp/2, 3, "[ft_br]"}
  defp to_basic_units("[mi_br]"), do: {&mulp/2, 5280, "[ft_br]"}
  defp to_basic_units("[nmi_br]"), do: {&mulp/2, 6080, "[ft_br]"}
  defp to_basic_units("[kn_br]"), do: {&mulp/2, 1, "[nmi_br]/h"}
  defp to_basic_units("[acr_br]"), do: {&mulp/2, 4840, "[yd_br]2"}
  defp to_basic_units("[gal_us]"), do: {&mulp/2, 231, "[in_i]3"}
  defp to_basic_units("[bbl_us]"), do: {&mulp/2, 42, "[gal_us]"}
  defp to_basic_units("[qt_us]"), do: {&mulp/2, 1, "[gal_us]/4"}
  defp to_basic_units("[pt_us]"), do: {&mulp/2, 1, "[qt_us]/2"}
  defp to_basic_units("[gil_us]"), do: {&mulp/2, 1, "[pt_us]/4"}
  defp to_basic_units("[foz_us]"), do: {&mulp/2, 1, "[gil_us]/4"}
  defp to_basic_units("[fdr_us]"), do: {&mulp/2, 1, "[foz_us]/8"}
  defp to_basic_units("[min_us]"), do: {&mulp/2, 1, "[fdr_us]/60"}
  defp to_basic_units("[crd_us]"), do: {&mulp/2, 128, "[ft_i]3"}
  defp to_basic_units("[bu_us]"), do: {&mulp/2, 2150.42, "[in_i]3"}
  defp to_basic_units("[gal_wi]"), do: {&mulp/2, 1, "[bu_us]/8"}
  defp to_basic_units("[pk_us]"), do: {&mulp/2, 1, "[bu_us]/4"}
  defp to_basic_units("[dqt_us]"), do: {&mulp/2, 1, "[pk_us]/8"}
  defp to_basic_units("[dpt_us]"), do: {&mulp/2, 1, "[dqt_us]/2"}
  defp to_basic_units("[tbs_us]"), do: {&mulp/2, 1, "[foz_us]/2"}
  defp to_basic_units("[tsp_us]"), do: {&mulp/2, 1, "[tbs_us]/3"}
  defp to_basic_units("[cup_us]"), do: {&mulp/2, 16, "[tbs_us]"}
  defp to_basic_units("[foz_m]"), do: {&mulp/2, 30, "mL"}
  defp to_basic_units("[cup_m]"), do: {&mulp/2, 240, "mL"}
  defp to_basic_units("[tsp_m]"), do: {&mulp/2, 5, "mL"}
  defp to_basic_units("[tbs_m]"), do: {&mulp/2, 15, "mL"}
  defp to_basic_units("[gal_br]"), do: {&mulp/2, 4.54609, "l"}
  defp to_basic_units("[pk_br]"), do: {&mulp/2, 2, "[gal_br]"}
  defp to_basic_units("[bu_br]"), do: {&mulp/2, 4, "[pk_br]"}
  defp to_basic_units("[qt_br]"), do: {&mulp/2, 1, "[gal_br]/4"}
  defp to_basic_units("[pt_br]"), do: {&mulp/2, 1, "[qt_br]/2"}
  defp to_basic_units("[gil_br]"), do: {&mulp/2, 1, "[pt_br]/4"}
  defp to_basic_units("[foz_br]"), do: {&mulp/2, 1, "[gil_br]/5"}
  defp to_basic_units("[fdr_br]"), do: {&mulp/2, 1, "[foz_br]/8"}
  defp to_basic_units("[min_br]"), do: {&mulp/2, 1, "[fdr_br]/60"}
  defp to_basic_units("[gr]"), do: {&mulp/2, 64.79891, "mg"}
  defp to_basic_units("[lb_av]"), do: {&mulp/2, 7000, "[gr]"}
  defp to_basic_units("[lbf_av]"), do: {&mulp/2, 1, "[lb_av].[g]"}
  defp to_basic_units("[oz_av]"), do: {&mulp/2, 1, "[lb_av]/16"}
  defp to_basic_units("[dr_av]"), do: {&mulp/2, 1, "[oz_av]/16"}
  defp to_basic_units("[scwt_av]"), do: {&mulp/2, 100, "[lb_av]"}
  defp to_basic_units("[lcwt_av]"), do: {&mulp/2, 112, "[lb_av]"}
  defp to_basic_units("[ston_av]"), do: {&mulp/2, 20, "[scwt_av]"}
  defp to_basic_units("[lton_av]"), do: {&mulp/2, 20, "[lcwt_av]"}
  defp to_basic_units("[stone_av]"), do: {&mulp/2, 14, "[lb_av]"}
  defp to_basic_units("[pwt_tr]"), do: {&mulp/2, 24, "[gr]"}
  defp to_basic_units("[oz_tr]"), do: {&mulp/2, 20, "[pwt_tr]"}
  defp to_basic_units("[lb_tr]"), do: {&mulp/2, 12, "[oz_tr]"}
  defp to_basic_units("[sc_ap]"), do: {&mulp/2, 20, "[gr]"}
  defp to_basic_units("[dr_ap]"), do: {&mulp/2, 3, "[sc_ap]"}
  defp to_basic_units("[oz_ap]"), do: {&mulp/2, 8, "[dr_ap]"}
  defp to_basic_units("[lb_ap]"), do: {&mulp/2, 12, "[oz_ap]"}
  defp to_basic_units("[oz_m]"), do: {&mulp/2, 28, "g"}
  defp to_basic_units("[lne]"), do: {&mulp/2, 1, "[in_i]/12"}
  defp to_basic_units("[pnt]"), do: {&mulp/2, 1, "[lne]/6"}
  defp to_basic_units("[pca]"), do: {&mulp/2, 12, "[pnt]"}
  defp to_basic_units("[pnt_pr]"), do: {&mulp/2, 0.013837, "[in_i]"}
  defp to_basic_units("[pca_pr]"), do: {&mulp/2, 12, "[pnt_pr]"}
  defp to_basic_units("[pied]"), do: {&mulp/2, 32.48, "cm"}
  defp to_basic_units("[pouce]"), do: {&mulp/2, 1, "[pied]/12"}
  defp to_basic_units("[ligne]"), do: {&mulp/2, 1, "[pouce]/12"}
  defp to_basic_units("[didot]"), do: {&mulp/2, 1, "[ligne]/6"}
  defp to_basic_units("[cicero]"), do: {&mulp/2, 12, "[didot]"}
  defp to_basic_units("[degR]"), do: {&mulp/2, 5, "K/9"}
  defp to_basic_units("cal_[15]"), do: {&mulp/2, 4.18580, "J"}
  defp to_basic_units("cal_[20]"), do: {&mulp/2, 4.18190, "J"}
  defp to_basic_units("cal_m"), do: {&mulp/2, 4.19002, "J"}
  defp to_basic_units("cal_IT"), do: {&mulp/2, 4.1868, "J"}
  defp to_basic_units("cal_th"), do: {&mulp/2, 4.184, "J"}
  defp to_basic_units("cal"), do: {&mulp/2, 1, "cal_th"}
  defp to_basic_units("[Cal]"), do: {&mulp/2, 1, "kcal_th"}
  defp to_basic_units("[Btu_39]"), do: {&mulp/2, 1.05967, "kJ"}
  defp to_basic_units("[Btu_59]"), do: {&mulp/2, 1.05480, "kJ"}
  defp to_basic_units("[Btu_60]"), do: {&mulp/2, 1.05468, "kJ"}
  defp to_basic_units("[Btu_m]"), do: {&mulp/2, 1.05587, "kJ"}
  defp to_basic_units("[Btu_IT]"), do: {&mulp/2, 1.05505585262, "kJ"}
  defp to_basic_units("[Btu_th]"), do: {&mulp/2, 1.054350, "kJ"}
  defp to_basic_units("[Btu]"), do: {&mulp/2, 1, "[Btu_th]"}
  defp to_basic_units("[HP]"), do: {&mulp/2, 550, "[ft_i].[lbf_av]/s"}
  defp to_basic_units("tex"), do: {&mulp/2, 1, "g/km"}
  defp to_basic_units("[den]"), do: {&mulp/2, 1, "g/9/km"}
  defp to_basic_units("m[H2O]"), do: {&mulp/2, 980665.0e-5, "kPa"}
  defp to_basic_units("m[Hg]"), do: {&mulp/2, 133.3220, "kPa"}
  defp to_basic_units("[in_i'H2O]"), do: {&mulp/2, 1, "m[H2O].[in_i]/m"}
  defp to_basic_units("[in_i'Hg]"), do: {&mulp/2, 1, "m[Hg].[in_i]/m"}
  defp to_basic_units("[PRU]"), do: {&mulp/2, 1, "mm[Hg].s/ml"}
  defp to_basic_units("[wood'U]"), do: {&mulp/2, 1, "mm[Hg].min/L"}
  defp to_basic_units("[diop]"), do: {&mulp/2, 1, "/m"}
  defp to_basic_units("[mesh_i]"), do: {&mulp/2, 1, "/[in_i]"}
  defp to_basic_units("[Ch]"), do: {&mulp/2, 1, "mm/3"}
  defp to_basic_units("[drp]"), do: {&mulp/2, 1, "ml/20"}
  defp to_basic_units("[hnsf'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[MET]"), do: {&mulp/2, 3.5, "mL/min/kg"}
  defp to_basic_units("[hp_X]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[hp_C]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[hp_M]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[hp_Q]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[kp_X]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[kp_C]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[kp_M]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[kp_Q]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("eq"), do: {&mulp/2, 1, "mol"}
  defp to_basic_units("osm"), do: {&mulp/2, 1, "mol"}
  defp to_basic_units("g%"), do: {&mulp/2, 1, "g/dl"}
  defp to_basic_units("[S]"), do: {&mulp/2, 1, "10*-13.s"}
  defp to_basic_units("[HPF]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[LPF]"), do: {&mulp/2, 100, "1"}
  defp to_basic_units("kat"), do: {&mulp/2, 1, "mol/s"}
  defp to_basic_units("U"), do: {&mulp/2, 1, "umol/min"}
  defp to_basic_units("[iU]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[IU]"), do: {&mulp/2, 1, "[iU]"}
  defp to_basic_units("[arb'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[USP'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[GPL'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[MPL'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[APL'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[beth'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[anti'Xa'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[todd'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[dye'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[smgy'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[bdsk'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[ka'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[knk'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[mclg'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[tb'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[CCID_50]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[TCID_50]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[EID_50]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[PFU]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[FFU]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[CFU]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[IR]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[BAU]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[AU]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[Amb'a'1'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[PNU]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[Lf]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[D'ag'U]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[FEU]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[ELU]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("[EU]"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("st"), do: {&mulp/2, 1, "m3"}
  defp to_basic_units("Ao"), do: {&mulp/2, 0.1, "nm"}
  defp to_basic_units("b"), do: {&mulp/2, 100, "fm2"}
  defp to_basic_units("att"), do: {&mulp/2, 1, "kgf/cm2"}
  defp to_basic_units("mho"), do: {&mulp/2, 1, "S"}
  defp to_basic_units("[psi]"), do: {&mulp/2, 1, "[lbf_av]/[in_i]2"}
  defp to_basic_units("circ"), do: {&mulp/2, 2, "[pi].rad"}
  defp to_basic_units("sph"), do: {&mulp/2, 4, "[pi].sr"}
  defp to_basic_units("[car_m]"), do: {&mulp/2, 2.0e-1, "g"}
  defp to_basic_units("[car_Au]"), do: {&mulp/2, 1, "/24"}
  defp to_basic_units("[smoot]"), do: {&mulp/2, 67, "[in_i]"}
  defp to_basic_units("bit"), do: {&mulp/2, 1, "1"}
  defp to_basic_units("By"), do: {&mulp/2, 8, "bit"}
  defp to_basic_units("Bd"), do: {&mulp/2, 1, "/s"}
  defp to_basic_units("Cel"), do: {&cel/2, 273.15, "K"}
  defp to_basic_units("[degF]"), do: {&deg_f/2, 459.67, "K"}
  defp to_basic_units("[degRe]"), do: raise NotImplementedError
  defp to_basic_units("[p'diop]"), do: raise NotImplementedError
  defp to_basic_units("%[slope]"), do: raise NotImplementedError
  defp to_basic_units("[hp'_X]"), do: raise NotImplementedError
  defp to_basic_units("[hp'_C]"), do: raise NotImplementedError
  defp to_basic_units("[hp'_M]"), do: raise NotImplementedError
  defp to_basic_units("[hp'_Q]"), do: raise NotImplementedError
  defp to_basic_units("[pH]"), do: raise NotImplementedError
  defp to_basic_units("Np"), do: raise NotImplementedError
  defp to_basic_units("B"), do: raise NotImplementedError
  defp to_basic_units("B[SPL]"), do: raise NotImplementedError
  defp to_basic_units("B[V]"), do: raise NotImplementedError
  defp to_basic_units("B[mV]"), do: raise NotImplementedError
  defp to_basic_units("B[uV]"), do: raise NotImplementedError
  defp to_basic_units("B[10.nV]"), do: raise NotImplementedError
  defp to_basic_units("B[W]"), do: raise NotImplementedError
  defp to_basic_units("B[kW]"), do: raise NotImplementedError
  defp to_basic_units("[m/s2/Hz^(1/2)]"), do: raise NotImplementedError
  defp to_basic_units("bit_s"), do: raise NotImplementedError
  defp to_basic_units(_unit), do: {&mulp/2, 1, "1"}
end
