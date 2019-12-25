Definitions.

OPEN             = \(
CLOSE            = \)
SQUARE_OPEN      = \[
SQUARE_CLOSE     = \]
PERIOD           = \.
SOLIDUS          = \/
SIGN             = [\+\-]

DIGITS           = [0-9]+
EXPONENT         = ([\+\-][0-9]+|[0-9]+)
PREFIX           = (k|Y|m|Z|E|P|T|G|M|h|d|a|d|c|u|n|p|f|a|z|y|(Ki)|(Mi)|(Gi)|(Ti))
ATOM_UNIT        = ((10\*)|(10\^)|(\[pi\])|%|(\[ppth\])|(\[ppm\])|(\[ppb\])|(\[pptr\])|(gon)|(deg)|'|('')|(min)|h|d|(a_t)|(a_j)|(a_g)|a|(wk)|(mo_s)|(mo_j)|(mo_g)|(mo)|(AU)|(atm)|(\[lbf_av\])|(\[in_i\])|(\[ft_i\])|(\[yd_i\])|(\[mi_i\])|(\[fth_i\])|(\[nmi_i\])|(\[kn_i\])|(\[sin_i\])|(\[sft_i\])|(\[syd_i\])|(\[cin_i\])|(\[cft_i\])|(\[cyd_i\])|(\[bf_i\])|(\[cr_i\])|(\[mil_i\])|(\[cml_i\])|(\[hd_i\])|(\[ft_us\])|(\[yd_us\])|(\[in_us\])|(\[rd_us\])|(\[ch_us\])|(\[lk_us\])|(\[rch_us\])|(\[rlk_us\])|(\[fth_us\])|(\[fur_us\])|(\[mi_us\])|(\[acr_us\])|(\[srd_us\])|(\[smi_us\])|(\[sct\])|(\[twp\])|(\[mil_us\])|(\[in_br\])|(\[ft_br\])|(\[rd_br\])|(\[ch_br\])|(\[lk_br\])|(\[fth_br\])|(\[pc_br\])|(\[yd_br\])|(\[mi_br\])|(\[nmi_br\])|(\[kn_br\])|(\[acr_br\])|(\[gal_us\])|(\[bbl_us\])|(\[qt_us\])|(\[pt_us\])|(\[gil_us\])|(\[foz_us\])|(\[fdr_us\])|(\[min_us\])|(\[crd_us\])|(\[bu_us\])|(\[gal_wi\])|(\[pk_us\])|(\[dqt_us\])|(\[dpt_us\])|(\[tbs_us\])|(\[tsp_us\])|(\[cup_us\])|(\[foz_m\])|(\[cup_m\])|(\[tsp_m\])|(\[tbs_m\])|(\[gal_br\])|(\[pk_br\])|(\[bu_br\])|(\[qt_br\])|(\[pt_br\])|(\[gil_br\])|(\[foz_br\])|(\[fdr_br\])|(\[min_br\])|(\[gr\])|(\[lb_av\])|(\[oz_av\])|(\[dr_av\])|(\[scwt_av\])|(\[lcwt_av\])|(\[ston_av\])|(\[lton_av\])|(\[stone_av\])|(\[pwt_tr\])|(\[oz_tr\])|(\[lb_tr\])|(\[sc_ap\])|(\[dr_ap\])|(\[oz_ap\])|(\[lb_ap\])|(\[oz_m\])|(\[lne\])|(\[pnt\])|(\[pca\])|(\[pnt_pr\])|(\[pca_pr\])|(\[pied\])|(\[pouce\])|(\[ligne\])|(\[didot\])|(\[cicero\])|(\[degF\])|(\[degR\])|(\[degRe\])|(\[Cal\])|(\[Btu_39\])|(\[Btu_59\])|(\[Btu_60\])|(\[Btu_m\])|(\[Btu_IT\])|(\[Btu_th\])|(\[Btu\])|(\[HP\])|(\[den\])|(\[in_i'H2O\])|(\[in_i'Hg\])|(\[PRU\])|(\[wood'U\])|(\[diop\])|(\[p'diop\])|(%\[slope\])|(\[mesh_i\])|(\[Ch\])|(\[drp\])|(\[hnsf'U\])|(\[MET\])|(\[hp'_X\])|(\[hp'_C\])|(\[hp'_M\])|(\[hp'_Q\])|(\[hp_X\])|(\[hp_C\])|(\[hp_M\])|(\[hp_Q\])|(\[kp_X\])|(\[kp_C\])|(\[kp_M\])|(\[kp_Q\])|(\[pH\])|(\[S\])|(\[HPF\])|(\[LPF\])|(\[arb'U\])|(\[USP'U\])|(\[GPL'U\])|(\[MPL'U\])|(\[APL'U\])|(\[beth'U\])|(\[anti'Xa'U\])|(\[todd'U\])|(\[dye'U\])|(\[smgy'U\])|(\[bdsk'U\])|(\[ka'U\])|(\[knk'U\])|(\[mclg'U\])|(\[tb'U\])|(\[CCID_50\])|(\[TCID_50\])|(\[EID_50\])|(\[PFU\])|(\[FFU\])|(\[CFU\])|(\[IR\])|(\[BAU\])|(\[AU\])|(\[Amb'a'1'U\])|(\[PNU\])|(\[Lf\])|(\[D'ag'U\])|(\[FEU\])|(\[ELU\])|(\[EU\])|(Ao)|b|(att)|(\[psi\])|(circ)|(sph)|(\[car_m\])|(\[car_Au\])|(\[smoot\])|(\[m\/s2\/Hz\^\(1\/2\)\])|(bit_s))
METRIC_ATOM_UNIT = (m|s|g|(rad)|K|C|(cd)|(mol)|(sr)|(Hz)|N|(Pa)|J|W|A|V|F|(Ohm)|S|(Wb)|(Cel)|T|H|(lm)|(lx)|(Bq)|(Gy)|(Sv)|l|L|(ar)|t|(bar)|u|(eV)|(pc)|(\[c\])|(\[h\])|(\[k\])|(\[eps_0\])|(\[mu_0\])|(\[e\])|(\[m_e\])|(\[m_p\])|(\[G\])|(\[g\])|(\[ly\])|(gf)|(Ky)|(Gal)|(dyn)|(erg)|P|(Bi)|(St)|(Mx)|G|(Oe)|(Gb)|(sb)|(Lmb)|(ph)|(Ci)|R|(RAD)|(REM)|(cal_\[15\])|(cal_\[20\])|(cal_m)|(cal_IT)|(cal_th)|(cal)|(tex)|(m\[H2O\])|(m\[Hg\])|(eq)|(osm)|(g%)|(kat)|U|(\[iU\])|(\[IU\])|(Np)|B|(B\[SPL\])|(B\[V\])|(B\[mV\])|(B\[uV\])|(B\[10\.nV\])|(B\[W\])|(B\[kW\])|(st)|(mho)|(bit)|(By)|(Bd))

Rules.

{PREFIX}{METRIC_ATOM_UNIT}{EXPONENT}         : {token, {unit_exponent, handle_prefix_unit_exponent(TokenChars)}}.
(({ATOM_UNIT}|{METRIC_ATOM_UNIT}){EXPONENT}) : {token, {unit_exponent, handle_unit_exponent(TokenChars)}}.
{DIGITS}                                     : {token, {factor, list_to_integer(TokenChars)}}.
{OPEN}                                       : {token, {left_parent, list_to_binary(TokenChars)}}.
{CLOSE}                                      : {token, {right_parent, list_to_binary(TokenChars)}}.
{SQUARE_OPEN}                                : {token, {left_square, list_to_binary(TokenChars)}}.
{SQUARE_CLOSE}                               : {token, {right_square, list_to_binary(TokenChars)}}.
{PERIOD}                                     : {token, {period, list_to_binary(TokenChars)}}.
{SOLIDUS}                                    : {token, {solidus, list_to_binary(TokenChars)}}.
{SIGNS}                                      : {token, {sign, list_to_binary(TokenChars)}}.
{PREFIX}{METRIC_ATOM_UNIT}                   : {token, {prefix_atom_unit, prefix_atom_unit_to_bianry(TokenChars)}}.
{ATOM_UNIT}                                  : {token, {atom_unit, list_to_binary(TokenChars)}}.
{METRIC_ATOM_UNIT}                           : {token, {atom_unit, list_to_binary(TokenChars)}}.

Erlang code.

prefix_atom_unit_to_bianry([Prefix | Unit]) ->
    {list_to_binary([Prefix]), list_to_binary(Unit)}.

handle_prefix_unit_exponent([Prefix | UnitExponent]) ->
    {Unit, Exponent} = handle_unit_exponent(UnitExponent),
    {list_to_binary([Prefix]), Unit, Exponent}.

handle_unit_exponent(UnitExponent) ->
    UnitExponent2 = list_to_binary(UnitExponent),
    {match, [Match]} = re:run(UnitExponent2, <<"[\+\-]*[0-9]+$">>),
    Exponent = binary:part(UnitExponent2, Match),
    Unit = binary:replace(UnitExponent2, Exponent, <<"">>),

    {Unit, binary_to_integer(Exponent)}.
