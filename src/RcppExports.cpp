// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_calc_dist_reef
Rcpp::NumericVector rcpp_calc_dist_reef(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector extent, bool torus);
RcppExport SEXP _arrR_rcpp_calc_dist_reef(SEXP seafloorSEXP, SEXP coords_reefSEXP, SEXP extentSEXP, SEXP torusSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type coords_reef(coords_reefSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    Rcpp::traits::input_parameter< bool >::type torus(torusSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calc_dist_reef(seafloor, coords_reef, extent, torus));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_calc_fishpop_growth
void rcpp_calc_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, Rcpp::NumericMatrix seafloor, Rcpp::NumericVector fish_id, Rcpp::NumericVector cell_id, Rcpp::NumericVector pop_thres_reserves, double pop_k, double pop_linf, double pop_a, double pop_b, double pop_n_body, double pop_max_reserves, double pop_want_reserves, double min_per_i);
RcppExport SEXP _arrR_rcpp_calc_fishpop_growth(SEXP fishpopSEXP, SEXP fishpop_trackSEXP, SEXP seafloorSEXP, SEXP fish_idSEXP, SEXP cell_idSEXP, SEXP pop_thres_reservesSEXP, SEXP pop_kSEXP, SEXP pop_linfSEXP, SEXP pop_aSEXP, SEXP pop_bSEXP, SEXP pop_n_bodySEXP, SEXP pop_max_reservesSEXP, SEXP pop_want_reservesSEXP, SEXP min_per_iSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop_track(fishpop_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type fish_id(fish_idSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type cell_id(cell_idSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type pop_thres_reserves(pop_thres_reservesSEXP);
    Rcpp::traits::input_parameter< double >::type pop_k(pop_kSEXP);
    Rcpp::traits::input_parameter< double >::type pop_linf(pop_linfSEXP);
    Rcpp::traits::input_parameter< double >::type pop_a(pop_aSEXP);
    Rcpp::traits::input_parameter< double >::type pop_b(pop_bSEXP);
    Rcpp::traits::input_parameter< double >::type pop_n_body(pop_n_bodySEXP);
    Rcpp::traits::input_parameter< double >::type pop_max_reserves(pop_max_reservesSEXP);
    Rcpp::traits::input_parameter< double >::type pop_want_reserves(pop_want_reservesSEXP);
    Rcpp::traits::input_parameter< double >::type min_per_i(min_per_iSEXP);
    rcpp_calc_fishpop_growth(fishpop, fishpop_track, seafloor, fish_id, cell_id, pop_thres_reserves, pop_k, pop_linf, pop_a, pop_b, pop_n_body, pop_max_reserves, pop_want_reserves, min_per_i);
    return R_NilValue;
END_RCPP
}
// rcpp_calc_mineralization
void rcpp_calc_mineralization(Rcpp::NumericMatrix seafloor, double detritus_dead_ratio, double detritus_mineralization);
RcppExport SEXP _arrR_rcpp_calc_mineralization(SEXP seafloorSEXP, SEXP detritus_dead_ratioSEXP, SEXP detritus_mineralizationSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_dead_ratio(detritus_dead_ratioSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_mineralization(detritus_mineralizationSEXP);
    rcpp_calc_mineralization(seafloor, detritus_dead_ratio, detritus_mineralization);
    return R_NilValue;
END_RCPP
}
// rcpp_calc_mortality_background
void rcpp_calc_mortality_background(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, Rcpp::NumericMatrix seafloor, Rcpp::NumericVector fish_id, Rcpp::NumericVector cell_id, double pop_linf, double pop_n_body, double pop_want_reserves);
RcppExport SEXP _arrR_rcpp_calc_mortality_background(SEXP fishpopSEXP, SEXP fishpop_trackSEXP, SEXP seafloorSEXP, SEXP fish_idSEXP, SEXP cell_idSEXP, SEXP pop_linfSEXP, SEXP pop_n_bodySEXP, SEXP pop_want_reservesSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop_track(fishpop_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type fish_id(fish_idSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type cell_id(cell_idSEXP);
    Rcpp::traits::input_parameter< double >::type pop_linf(pop_linfSEXP);
    Rcpp::traits::input_parameter< double >::type pop_n_body(pop_n_bodySEXP);
    Rcpp::traits::input_parameter< double >::type pop_want_reserves(pop_want_reservesSEXP);
    rcpp_calc_mortality_background(fishpop, fishpop_track, seafloor, fish_id, cell_id, pop_linf, pop_n_body, pop_want_reserves);
    return R_NilValue;
END_RCPP
}
// rcpp_calc_respiration
void rcpp_calc_respiration(Rcpp::NumericMatrix fishpop, double resp_intercept, double resp_slope, double resp_temp_low, double resp_temp_max, double resp_temp_optm, double water_temp, double min_per_i);
RcppExport SEXP _arrR_rcpp_calc_respiration(SEXP fishpopSEXP, SEXP resp_interceptSEXP, SEXP resp_slopeSEXP, SEXP resp_temp_lowSEXP, SEXP resp_temp_maxSEXP, SEXP resp_temp_optmSEXP, SEXP water_tempSEXP, SEXP min_per_iSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< double >::type resp_intercept(resp_interceptSEXP);
    Rcpp::traits::input_parameter< double >::type resp_slope(resp_slopeSEXP);
    Rcpp::traits::input_parameter< double >::type resp_temp_low(resp_temp_lowSEXP);
    Rcpp::traits::input_parameter< double >::type resp_temp_max(resp_temp_maxSEXP);
    Rcpp::traits::input_parameter< double >::type resp_temp_optm(resp_temp_optmSEXP);
    Rcpp::traits::input_parameter< double >::type water_temp(water_tempSEXP);
    Rcpp::traits::input_parameter< double >::type min_per_i(min_per_iSEXP);
    rcpp_calc_respiration(fishpop, resp_intercept, resp_slope, resp_temp_low, resp_temp_max, resp_temp_optm, water_temp, min_per_i);
    return R_NilValue;
END_RCPP
}
// rcpp_convert_nutr
double rcpp_convert_nutr(double x, String to);
RcppExport SEXP _arrR_rcpp_convert_nutr(SEXP xSEXP, SEXP toSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< String >::type to(toSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_convert_nutr(x, to));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_calc_nutr_uptake
double rcpp_calc_nutr_uptake(double nutrients, double biomass, double v_max, double k_m, double time_frac);
RcppExport SEXP _arrR_rcpp_calc_nutr_uptake(SEXP nutrientsSEXP, SEXP biomassSEXP, SEXP v_maxSEXP, SEXP k_mSEXP, SEXP time_fracSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type nutrients(nutrientsSEXP);
    Rcpp::traits::input_parameter< double >::type biomass(biomassSEXP);
    Rcpp::traits::input_parameter< double >::type v_max(v_maxSEXP);
    Rcpp::traits::input_parameter< double >::type k_m(k_mSEXP);
    Rcpp::traits::input_parameter< double >::type time_frac(time_fracSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calc_nutr_uptake(nutrients, biomass, v_max, k_m, time_frac));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_calc_seagrass_growth
void rcpp_calc_seagrass_growth(Rcpp::NumericMatrix seafloor, Rcpp::NumericVector cells_reef, double bg_v_max, double bg_k_m, double bg_gamma, double ag_v_max, double ag_k_m, double ag_gamma, double bg_biomass_max, double bg_biomass_min, double ag_biomass_max, double ag_biomass_min, double detritus_ratio, double bg_thres, double min_per_i);
RcppExport SEXP _arrR_rcpp_calc_seagrass_growth(SEXP seafloorSEXP, SEXP cells_reefSEXP, SEXP bg_v_maxSEXP, SEXP bg_k_mSEXP, SEXP bg_gammaSEXP, SEXP ag_v_maxSEXP, SEXP ag_k_mSEXP, SEXP ag_gammaSEXP, SEXP bg_biomass_maxSEXP, SEXP bg_biomass_minSEXP, SEXP ag_biomass_maxSEXP, SEXP ag_biomass_minSEXP, SEXP detritus_ratioSEXP, SEXP bg_thresSEXP, SEXP min_per_iSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type cells_reef(cells_reefSEXP);
    Rcpp::traits::input_parameter< double >::type bg_v_max(bg_v_maxSEXP);
    Rcpp::traits::input_parameter< double >::type bg_k_m(bg_k_mSEXP);
    Rcpp::traits::input_parameter< double >::type bg_gamma(bg_gammaSEXP);
    Rcpp::traits::input_parameter< double >::type ag_v_max(ag_v_maxSEXP);
    Rcpp::traits::input_parameter< double >::type ag_k_m(ag_k_mSEXP);
    Rcpp::traits::input_parameter< double >::type ag_gamma(ag_gammaSEXP);
    Rcpp::traits::input_parameter< double >::type bg_biomass_max(bg_biomass_maxSEXP);
    Rcpp::traits::input_parameter< double >::type bg_biomass_min(bg_biomass_minSEXP);
    Rcpp::traits::input_parameter< double >::type ag_biomass_max(ag_biomass_maxSEXP);
    Rcpp::traits::input_parameter< double >::type ag_biomass_min(ag_biomass_minSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_ratio(detritus_ratioSEXP);
    Rcpp::traits::input_parameter< double >::type bg_thres(bg_thresSEXP);
    Rcpp::traits::input_parameter< double >::type min_per_i(min_per_iSEXP);
    rcpp_calc_seagrass_growth(seafloor, cells_reef, bg_v_max, bg_k_m, bg_gamma, ag_v_max, ag_k_m, ag_gamma, bg_biomass_max, bg_biomass_min, ag_biomass_max, ag_biomass_min, detritus_ratio, bg_thres, min_per_i);
    return R_NilValue;
END_RCPP
}
// rcpp_cell_from_xy
int rcpp_cell_from_xy(Rcpp::NumericVector coords, Rcpp::NumericVector dimensions, Rcpp::NumericVector extent);
RcppExport SEXP _arrR_rcpp_cell_from_xy(SEXP coordsSEXP, SEXP dimensionsSEXP, SEXP extentSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type coords(coordsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dimensions(dimensionsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_cell_from_xy(coords, dimensions, extent));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_diffuse_values
void rcpp_diffuse_values(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix cell_adj, double nutrients_diffusion, double detritus_diffusion, double detritus_dead_diffusion);
RcppExport SEXP _arrR_rcpp_diffuse_values(SEXP seafloorSEXP, SEXP cell_adjSEXP, SEXP nutrients_diffusionSEXP, SEXP detritus_diffusionSEXP, SEXP detritus_dead_diffusionSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cell_adj(cell_adjSEXP);
    Rcpp::traits::input_parameter< double >::type nutrients_diffusion(nutrients_diffusionSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_diffusion(detritus_diffusionSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_dead_diffusion(detritus_dead_diffusionSEXP);
    rcpp_diffuse_values(seafloor, cell_adj, nutrients_diffusion, detritus_diffusion, detritus_dead_diffusion);
    return R_NilValue;
END_RCPP
}
// rcpp_modify_degree
double rcpp_modify_degree(double x, double y);
RcppExport SEXP _arrR_rcpp_modify_degree(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_modify_degree(x, y));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_move_fishpop
void rcpp_move_fishpop(Rcpp::NumericMatrix fishpop, Rcpp::NumericVector reef_dist, Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector pop_thres_reserves, double move_mean, double move_reef, double move_return, Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);
RcppExport SEXP _arrR_rcpp_move_fishpop(SEXP fishpopSEXP, SEXP reef_distSEXP, SEXP coords_reefSEXP, SEXP pop_thres_reservesSEXP, SEXP move_meanSEXP, SEXP move_reefSEXP, SEXP move_returnSEXP, SEXP extentSEXP, SEXP dimensionsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type reef_dist(reef_distSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type coords_reef(coords_reefSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type pop_thres_reserves(pop_thres_reservesSEXP);
    Rcpp::traits::input_parameter< double >::type move_mean(move_meanSEXP);
    Rcpp::traits::input_parameter< double >::type move_reef(move_reefSEXP);
    Rcpp::traits::input_parameter< double >::type move_return(move_returnSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dimensions(dimensionsSEXP);
    rcpp_move_fishpop(fishpop, reef_dist, coords_reef, pop_thres_reserves, move_mean, move_reef, move_return, extent, dimensions);
    return R_NilValue;
END_RCPP
}
// rcpp_reincarnate
void rcpp_reincarnate(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, Rcpp::NumericMatrix seafloor, int fish_id, int cell_id, double pop_linf, double pop_n_body, double pop_want_reserves, String reason);
RcppExport SEXP _arrR_rcpp_reincarnate(SEXP fishpopSEXP, SEXP fishpop_trackSEXP, SEXP seafloorSEXP, SEXP fish_idSEXP, SEXP cell_idSEXP, SEXP pop_linfSEXP, SEXP pop_n_bodySEXP, SEXP pop_want_reservesSEXP, SEXP reasonSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop_track(fishpop_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< int >::type fish_id(fish_idSEXP);
    Rcpp::traits::input_parameter< int >::type cell_id(cell_idSEXP);
    Rcpp::traits::input_parameter< double >::type pop_linf(pop_linfSEXP);
    Rcpp::traits::input_parameter< double >::type pop_n_body(pop_n_bodySEXP);
    Rcpp::traits::input_parameter< double >::type pop_want_reserves(pop_want_reservesSEXP);
    Rcpp::traits::input_parameter< String >::type reason(reasonSEXP);
    rcpp_reincarnate(fishpop, fishpop_track, seafloor, fish_id, cell_id, pop_linf, pop_n_body, pop_want_reserves, reason);
    return R_NilValue;
END_RCPP
}
// rcpp_rlognorm
double rcpp_rlognorm(double mean, double sd);
RcppExport SEXP _arrR_rcpp_rlognorm(SEXP meanSEXP, SEXP sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_rlognorm(mean, sd));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_translate_torus
Rcpp::NumericVector rcpp_translate_torus(Rcpp::NumericVector coords, Rcpp::NumericVector extent);
RcppExport SEXP _arrR_rcpp_translate_torus(SEXP coordsSEXP, SEXP extentSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type coords(coordsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_translate_torus(coords, extent));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_arrR_rcpp_calc_dist_reef", (DL_FUNC) &_arrR_rcpp_calc_dist_reef, 4},
    {"_arrR_rcpp_calc_fishpop_growth", (DL_FUNC) &_arrR_rcpp_calc_fishpop_growth, 14},
    {"_arrR_rcpp_calc_mineralization", (DL_FUNC) &_arrR_rcpp_calc_mineralization, 3},
    {"_arrR_rcpp_calc_mortality_background", (DL_FUNC) &_arrR_rcpp_calc_mortality_background, 8},
    {"_arrR_rcpp_calc_respiration", (DL_FUNC) &_arrR_rcpp_calc_respiration, 8},
    {"_arrR_rcpp_convert_nutr", (DL_FUNC) &_arrR_rcpp_convert_nutr, 2},
    {"_arrR_rcpp_calc_nutr_uptake", (DL_FUNC) &_arrR_rcpp_calc_nutr_uptake, 5},
    {"_arrR_rcpp_calc_seagrass_growth", (DL_FUNC) &_arrR_rcpp_calc_seagrass_growth, 15},
    {"_arrR_rcpp_cell_from_xy", (DL_FUNC) &_arrR_rcpp_cell_from_xy, 3},
    {"_arrR_rcpp_diffuse_values", (DL_FUNC) &_arrR_rcpp_diffuse_values, 5},
    {"_arrR_rcpp_modify_degree", (DL_FUNC) &_arrR_rcpp_modify_degree, 2},
    {"_arrR_rcpp_move_fishpop", (DL_FUNC) &_arrR_rcpp_move_fishpop, 9},
    {"_arrR_rcpp_reincarnate", (DL_FUNC) &_arrR_rcpp_reincarnate, 9},
    {"_arrR_rcpp_rlognorm", (DL_FUNC) &_arrR_rcpp_rlognorm, 2},
    {"_arrR_rcpp_translate_torus", (DL_FUNC) &_arrR_rcpp_translate_torus, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_arrR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
