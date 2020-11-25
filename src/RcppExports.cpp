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
void rcpp_calc_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, Rcpp::NumericMatrix seafloor, Rcpp::NumericVector fish_id, Rcpp::NumericVector cell_id, Rcpp::NumericMatrix growth_values, double pop_n_body, double pop_max_reserves, double pop_want_reserves, double min_per_i);
RcppExport SEXP _arrR_rcpp_calc_fishpop_growth(SEXP fishpopSEXP, SEXP fishpop_trackSEXP, SEXP seafloorSEXP, SEXP fish_idSEXP, SEXP cell_idSEXP, SEXP growth_valuesSEXP, SEXP pop_n_bodySEXP, SEXP pop_max_reservesSEXP, SEXP pop_want_reservesSEXP, SEXP min_per_iSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop_track(fishpop_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type fish_id(fish_idSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type cell_id(cell_idSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type growth_values(growth_valuesSEXP);
    Rcpp::traits::input_parameter< double >::type pop_n_body(pop_n_bodySEXP);
    Rcpp::traits::input_parameter< double >::type pop_max_reserves(pop_max_reservesSEXP);
    Rcpp::traits::input_parameter< double >::type pop_want_reserves(pop_want_reservesSEXP);
    Rcpp::traits::input_parameter< double >::type min_per_i(min_per_iSEXP);
    rcpp_calc_fishpop_growth(fishpop, fishpop_track, seafloor, fish_id, cell_id, growth_values, pop_n_body, pop_max_reserves, pop_want_reserves, min_per_i);
    return R_NilValue;
END_RCPP
}
// rcpp_calc_mineralization
void rcpp_calc_mineralization(Rcpp::NumericMatrix seafloor, double detritus_dead_decomp, double detritus_mineralization);
RcppExport SEXP _arrR_rcpp_calc_mineralization(SEXP seafloorSEXP, SEXP detritus_dead_decompSEXP, SEXP detritus_mineralizationSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_dead_decomp(detritus_dead_decompSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_mineralization(detritus_mineralizationSEXP);
    rcpp_calc_mineralization(seafloor, detritus_dead_decomp, detritus_mineralization);
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
// rcpp_check_max_biomass
Rcpp::NumericVector rcpp_check_max_biomass(double bg_biomass, double ag_biomass, double detritus_pool, double bg_biomass_max, double ag_biomass_max);
RcppExport SEXP _arrR_rcpp_check_max_biomass(SEXP bg_biomassSEXP, SEXP ag_biomassSEXP, SEXP detritus_poolSEXP, SEXP bg_biomass_maxSEXP, SEXP ag_biomass_maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type bg_biomass(bg_biomassSEXP);
    Rcpp::traits::input_parameter< double >::type ag_biomass(ag_biomassSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_pool(detritus_poolSEXP);
    Rcpp::traits::input_parameter< double >::type bg_biomass_max(bg_biomass_maxSEXP);
    Rcpp::traits::input_parameter< double >::type ag_biomass_max(ag_biomass_maxSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_check_max_biomass(bg_biomass, ag_biomass, detritus_pool, bg_biomass_max, ag_biomass_max));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_calc_seagrass_growth
void rcpp_calc_seagrass_growth(Rcpp::NumericMatrix seafloor, Rcpp::NumericVector cells_reef, double bg_v_max, double bg_k_m, double ag_v_max, double ag_k_m, double bg_biomass_max, double bg_biomass_min, double ag_biomass_max, double ag_biomass_min, double detritus_ratio, double bg_thres, double min_per_i);
RcppExport SEXP _arrR_rcpp_calc_seagrass_growth(SEXP seafloorSEXP, SEXP cells_reefSEXP, SEXP bg_v_maxSEXP, SEXP bg_k_mSEXP, SEXP ag_v_maxSEXP, SEXP ag_k_mSEXP, SEXP bg_biomass_maxSEXP, SEXP bg_biomass_minSEXP, SEXP ag_biomass_maxSEXP, SEXP ag_biomass_minSEXP, SEXP detritus_ratioSEXP, SEXP bg_thresSEXP, SEXP min_per_iSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type cells_reef(cells_reefSEXP);
    Rcpp::traits::input_parameter< double >::type bg_v_max(bg_v_maxSEXP);
    Rcpp::traits::input_parameter< double >::type bg_k_m(bg_k_mSEXP);
    Rcpp::traits::input_parameter< double >::type ag_v_max(ag_v_maxSEXP);
    Rcpp::traits::input_parameter< double >::type ag_k_m(ag_k_mSEXP);
    Rcpp::traits::input_parameter< double >::type bg_biomass_max(bg_biomass_maxSEXP);
    Rcpp::traits::input_parameter< double >::type bg_biomass_min(bg_biomass_minSEXP);
    Rcpp::traits::input_parameter< double >::type ag_biomass_max(ag_biomass_maxSEXP);
    Rcpp::traits::input_parameter< double >::type ag_biomass_min(ag_biomass_minSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_ratio(detritus_ratioSEXP);
    Rcpp::traits::input_parameter< double >::type bg_thres(bg_thresSEXP);
    Rcpp::traits::input_parameter< double >::type min_per_i(min_per_iSEXP);
    rcpp_calc_seagrass_growth(seafloor, cells_reef, bg_v_max, bg_k_m, ag_v_max, ag_k_m, bg_biomass_max, bg_biomass_min, ag_biomass_max, ag_biomass_min, detritus_ratio, bg_thres, min_per_i);
    return R_NilValue;
END_RCPP
}
// rcpp_create_rebirth
void rcpp_create_rebirth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, Rcpp::NumericMatrix seafloor, Rcpp::NumericVector fish_id, Rcpp::NumericVector cell_id, double pop_n_body, double pop_want_reserves);
RcppExport SEXP _arrR_rcpp_create_rebirth(SEXP fishpopSEXP, SEXP fishpop_trackSEXP, SEXP seafloorSEXP, SEXP fish_idSEXP, SEXP cell_idSEXP, SEXP pop_n_bodySEXP, SEXP pop_want_reservesSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop_track(fishpop_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type fish_id(fish_idSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type cell_id(cell_idSEXP);
    Rcpp::traits::input_parameter< double >::type pop_n_body(pop_n_bodySEXP);
    Rcpp::traits::input_parameter< double >::type pop_want_reserves(pop_want_reservesSEXP);
    rcpp_create_rebirth(fishpop, fishpop_track, seafloor, fish_id, cell_id, pop_n_body, pop_want_reserves);
    return R_NilValue;
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

static const R_CallMethodDef CallEntries[] = {
    {"_arrR_rcpp_calc_dist_reef", (DL_FUNC) &_arrR_rcpp_calc_dist_reef, 4},
    {"_arrR_rcpp_calc_fishpop_growth", (DL_FUNC) &_arrR_rcpp_calc_fishpop_growth, 10},
    {"_arrR_rcpp_calc_mineralization", (DL_FUNC) &_arrR_rcpp_calc_mineralization, 3},
    {"_arrR_rcpp_convert_nutr", (DL_FUNC) &_arrR_rcpp_convert_nutr, 2},
    {"_arrR_rcpp_calc_nutr_uptake", (DL_FUNC) &_arrR_rcpp_calc_nutr_uptake, 5},
    {"_arrR_rcpp_check_max_biomass", (DL_FUNC) &_arrR_rcpp_check_max_biomass, 5},
    {"_arrR_rcpp_calc_seagrass_growth", (DL_FUNC) &_arrR_rcpp_calc_seagrass_growth, 13},
    {"_arrR_rcpp_create_rebirth", (DL_FUNC) &_arrR_rcpp_create_rebirth, 7},
    {"_arrR_rcpp_diffuse_values", (DL_FUNC) &_arrR_rcpp_diffuse_values, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_arrR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
