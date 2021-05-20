// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_add_input
void rcpp_add_input(Rcpp::NumericMatrix seafloor, Rcpp::NumericVector nutr_input, int timestep);
RcppExport SEXP _arrR_rcpp_add_input(SEXP seafloorSEXP, SEXP nutr_inputSEXP, SEXP timestepSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type nutr_input(nutr_inputSEXP);
    Rcpp::traits::input_parameter< int >::type timestep(timestepSEXP);
    rcpp_add_input(seafloor, nutr_input, timestep);
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
// rcpp_closest_reef
Rcpp::NumericVector rcpp_closest_reef(Rcpp::NumericVector coords_temp, Rcpp::NumericMatrix coords_reef);
RcppExport SEXP _arrR_rcpp_closest_reef(SEXP coords_tempSEXP, SEXP coords_reefSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type coords_temp(coords_tempSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type coords_reef(coords_reefSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_closest_reef(coords_temp, coords_reef));
    return rcpp_result_gen;
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
// rcpp_diffuse_values
void rcpp_diffuse_values(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix cell_adj, double nutrients_diffusion, double detritus_diffusion, double detritus_fish_diffusion);
RcppExport SEXP _arrR_rcpp_diffuse_values(SEXP seafloorSEXP, SEXP cell_adjSEXP, SEXP nutrients_diffusionSEXP, SEXP detritus_diffusionSEXP, SEXP detritus_fish_diffusionSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cell_adj(cell_adjSEXP);
    Rcpp::traits::input_parameter< double >::type nutrients_diffusion(nutrients_diffusionSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_diffusion(detritus_diffusionSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_fish_diffusion(detritus_fish_diffusionSEXP);
    rcpp_diffuse_values(seafloor, cell_adj, nutrients_diffusion, detritus_diffusion, detritus_fish_diffusion);
    return R_NilValue;
END_RCPP
}
// rcpp_dist_reef
Rcpp::NumericVector rcpp_dist_reef(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector extent, bool torus);
RcppExport SEXP _arrR_rcpp_dist_reef(SEXP seafloorSEXP, SEXP coords_reefSEXP, SEXP extentSEXP, SEXP torusSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type coords_reef(coords_reefSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    Rcpp::traits::input_parameter< bool >::type torus(torusSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_dist_reef(seafloor, coords_reef, extent, torus));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_fishpop_growth
void rcpp_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, Rcpp::NumericMatrix seafloor, double pop_k, double pop_linf, double pop_a, double pop_b, double pop_n_body, double pop_want_reserves, double pop_max_reserves, double pop_consumption_prop, Rcpp::NumericVector extent, Rcpp::NumericVector dimensions, double min_per_i);
RcppExport SEXP _arrR_rcpp_fishpop_growth(SEXP fishpopSEXP, SEXP fishpop_trackSEXP, SEXP seafloorSEXP, SEXP pop_kSEXP, SEXP pop_linfSEXP, SEXP pop_aSEXP, SEXP pop_bSEXP, SEXP pop_n_bodySEXP, SEXP pop_want_reservesSEXP, SEXP pop_max_reservesSEXP, SEXP pop_consumption_propSEXP, SEXP extentSEXP, SEXP dimensionsSEXP, SEXP min_per_iSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop_track(fishpop_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< double >::type pop_k(pop_kSEXP);
    Rcpp::traits::input_parameter< double >::type pop_linf(pop_linfSEXP);
    Rcpp::traits::input_parameter< double >::type pop_a(pop_aSEXP);
    Rcpp::traits::input_parameter< double >::type pop_b(pop_bSEXP);
    Rcpp::traits::input_parameter< double >::type pop_n_body(pop_n_bodySEXP);
    Rcpp::traits::input_parameter< double >::type pop_want_reserves(pop_want_reservesSEXP);
    Rcpp::traits::input_parameter< double >::type pop_max_reserves(pop_max_reservesSEXP);
    Rcpp::traits::input_parameter< double >::type pop_consumption_prop(pop_consumption_propSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dimensions(dimensionsSEXP);
    Rcpp::traits::input_parameter< double >::type min_per_i(min_per_iSEXP);
    rcpp_fishpop_growth(fishpop, fishpop_track, seafloor, pop_k, pop_linf, pop_a, pop_b, pop_n_body, pop_want_reserves, pop_max_reserves, pop_consumption_prop, extent, dimensions, min_per_i);
    return R_NilValue;
END_RCPP
}
// rcpp_get_bearing
double rcpp_get_bearing(double x_fish, double y_fish, double x_reef, double y_reef);
RcppExport SEXP _arrR_rcpp_get_bearing(SEXP x_fishSEXP, SEXP y_fishSEXP, SEXP x_reefSEXP, SEXP y_reefSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x_fish(x_fishSEXP);
    Rcpp::traits::input_parameter< double >::type y_fish(y_fishSEXP);
    Rcpp::traits::input_parameter< double >::type x_reef(x_reefSEXP);
    Rcpp::traits::input_parameter< double >::type y_reef(y_reefSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_bearing(x_fish, y_fish, x_reef, y_reef));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_mineralization
void rcpp_mineralization(Rcpp::NumericMatrix seafloor, double detritus_fish_ratio, double detritus_mineralization);
RcppExport SEXP _arrR_rcpp_mineralization(SEXP seafloorSEXP, SEXP detritus_fish_ratioSEXP, SEXP detritus_mineralizationSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_fish_ratio(detritus_fish_ratioSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_mineralization(detritus_mineralizationSEXP);
    rcpp_mineralization(seafloor, detritus_fish_ratio, detritus_mineralization);
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
// rcpp_mortality
void rcpp_mortality(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, Rcpp::NumericMatrix seafloor, double pop_linf, double pop_n_body, double pop_want_reserves, Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);
RcppExport SEXP _arrR_rcpp_mortality(SEXP fishpopSEXP, SEXP fishpop_trackSEXP, SEXP seafloorSEXP, SEXP pop_linfSEXP, SEXP pop_n_bodySEXP, SEXP pop_want_reservesSEXP, SEXP extentSEXP, SEXP dimensionsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop_track(fishpop_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< double >::type pop_linf(pop_linfSEXP);
    Rcpp::traits::input_parameter< double >::type pop_n_body(pop_n_bodySEXP);
    Rcpp::traits::input_parameter< double >::type pop_want_reserves(pop_want_reservesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dimensions(dimensionsSEXP);
    rcpp_mortality(fishpop, fishpop_track, seafloor, pop_linf, pop_n_body, pop_want_reserves, extent, dimensions);
    return R_NilValue;
END_RCPP
}
// rcpp_move_behav
void rcpp_move_behav(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector pop_thres_reserves, double move_mean, double move_var, double move_reef, double move_border, double move_return, double max_dist, Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);
RcppExport SEXP _arrR_rcpp_move_behav(SEXP fishpopSEXP, SEXP coords_reefSEXP, SEXP pop_thres_reservesSEXP, SEXP move_meanSEXP, SEXP move_varSEXP, SEXP move_reefSEXP, SEXP move_borderSEXP, SEXP move_returnSEXP, SEXP max_distSEXP, SEXP extentSEXP, SEXP dimensionsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type coords_reef(coords_reefSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type pop_thres_reserves(pop_thres_reservesSEXP);
    Rcpp::traits::input_parameter< double >::type move_mean(move_meanSEXP);
    Rcpp::traits::input_parameter< double >::type move_var(move_varSEXP);
    Rcpp::traits::input_parameter< double >::type move_reef(move_reefSEXP);
    Rcpp::traits::input_parameter< double >::type move_border(move_borderSEXP);
    Rcpp::traits::input_parameter< double >::type move_return(move_returnSEXP);
    Rcpp::traits::input_parameter< double >::type max_dist(max_distSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dimensions(dimensionsSEXP);
    rcpp_move_behav(fishpop, coords_reef, pop_thres_reserves, move_mean, move_var, move_reef, move_border, move_return, max_dist, extent, dimensions);
    return R_NilValue;
END_RCPP
}
// rcpp_move_rand
void rcpp_move_rand(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef, double move_mean, double move_var, double move_visibility, double max_dist, bool reef_attraction, Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);
RcppExport SEXP _arrR_rcpp_move_rand(SEXP fishpopSEXP, SEXP coords_reefSEXP, SEXP move_meanSEXP, SEXP move_varSEXP, SEXP move_visibilitySEXP, SEXP max_distSEXP, SEXP reef_attractionSEXP, SEXP extentSEXP, SEXP dimensionsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type coords_reef(coords_reefSEXP);
    Rcpp::traits::input_parameter< double >::type move_mean(move_meanSEXP);
    Rcpp::traits::input_parameter< double >::type move_var(move_varSEXP);
    Rcpp::traits::input_parameter< double >::type move_visibility(move_visibilitySEXP);
    Rcpp::traits::input_parameter< double >::type max_dist(max_distSEXP);
    Rcpp::traits::input_parameter< bool >::type reef_attraction(reef_attractionSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dimensions(dimensionsSEXP);
    rcpp_move_rand(fishpop, coords_reef, move_mean, move_var, move_visibility, max_dist, reef_attraction, extent, dimensions);
    return R_NilValue;
END_RCPP
}
// rcpp_nutr_uptake
double rcpp_nutr_uptake(double nutrients, double biomass, double v_max, double k_m, double time_frac);
RcppExport SEXP _arrR_rcpp_nutr_uptake(SEXP nutrientsSEXP, SEXP biomassSEXP, SEXP v_maxSEXP, SEXP k_mSEXP, SEXP time_fracSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type nutrients(nutrientsSEXP);
    Rcpp::traits::input_parameter< double >::type biomass(biomassSEXP);
    Rcpp::traits::input_parameter< double >::type v_max(v_maxSEXP);
    Rcpp::traits::input_parameter< double >::type k_m(k_mSEXP);
    Rcpp::traits::input_parameter< double >::type time_frac(time_fracSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_nutr_uptake(nutrients, biomass, v_max, k_m, time_frac));
    return rcpp_result_gen;
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
// rcpp_remove_output
void rcpp_remove_output(Rcpp::NumericMatrix seafloor, double nutrients_output);
RcppExport SEXP _arrR_rcpp_remove_output(SEXP seafloorSEXP, SEXP nutrients_outputSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< double >::type nutrients_output(nutrients_outputSEXP);
    rcpp_remove_output(seafloor, nutrients_output);
    return R_NilValue;
END_RCPP
}
// rcpp_respiration
void rcpp_respiration(Rcpp::NumericMatrix fishpop, double resp_intercept, double resp_slope, double resp_temp_low, double resp_temp_max, double resp_temp_optm, double water_temp, double min_per_i);
RcppExport SEXP _arrR_rcpp_respiration(SEXP fishpopSEXP, SEXP resp_interceptSEXP, SEXP resp_slopeSEXP, SEXP resp_temp_lowSEXP, SEXP resp_temp_maxSEXP, SEXP resp_temp_optmSEXP, SEXP water_tempSEXP, SEXP min_per_iSEXP) {
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
    rcpp_respiration(fishpop, resp_intercept, resp_slope, resp_temp_low, resp_temp_max, resp_temp_optm, water_temp, min_per_i);
    return R_NilValue;
END_RCPP
}
// rcpp_rlognorm
double rcpp_rlognorm(double mean, double sd, double min, double max);
RcppExport SEXP _arrR_rcpp_rlognorm(SEXP meanSEXP, SEXP sdSEXP, SEXP minSEXP, SEXP maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< double >::type min(minSEXP);
    Rcpp::traits::input_parameter< double >::type max(maxSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_rlognorm(mean, sd, min, max));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_seagrass_growth
void rcpp_seagrass_growth(Rcpp::NumericMatrix seafloor, Rcpp::NumericVector cells_reef, double bg_v_max, double bg_k_m, double bg_gamma, double ag_v_max, double ag_k_m, double ag_gamma, double bg_biomass_max, double bg_biomass_min, double ag_biomass_max, double ag_biomass_min, double seagrass_thres, double seagrass_slope, double detritus_ratio, double time_frac);
RcppExport SEXP _arrR_rcpp_seagrass_growth(SEXP seafloorSEXP, SEXP cells_reefSEXP, SEXP bg_v_maxSEXP, SEXP bg_k_mSEXP, SEXP bg_gammaSEXP, SEXP ag_v_maxSEXP, SEXP ag_k_mSEXP, SEXP ag_gammaSEXP, SEXP bg_biomass_maxSEXP, SEXP bg_biomass_minSEXP, SEXP ag_biomass_maxSEXP, SEXP ag_biomass_minSEXP, SEXP seagrass_thresSEXP, SEXP seagrass_slopeSEXP, SEXP detritus_ratioSEXP, SEXP time_fracSEXP) {
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
    Rcpp::traits::input_parameter< double >::type seagrass_thres(seagrass_thresSEXP);
    Rcpp::traits::input_parameter< double >::type seagrass_slope(seagrass_slopeSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_ratio(detritus_ratioSEXP);
    Rcpp::traits::input_parameter< double >::type time_frac(time_fracSEXP);
    rcpp_seagrass_growth(seafloor, cells_reef, bg_v_max, bg_k_m, bg_gamma, ag_v_max, ag_k_m, ag_gamma, bg_biomass_max, bg_biomass_min, ag_biomass_max, ag_biomass_min, seagrass_thres, seagrass_slope, detritus_ratio, time_frac);
    return R_NilValue;
END_RCPP
}
// rcpp_shuffle
Rcpp::IntegerVector rcpp_shuffle(int min, int max);
RcppExport SEXP _arrR_rcpp_shuffle(SEXP minSEXP, SEXP maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type min(minSEXP);
    Rcpp::traits::input_parameter< int >::type max(maxSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_shuffle(min, max));
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
// rcpp_update_coords
void rcpp_update_coords(Rcpp::NumericMatrix fishpop, int i, double move_dist, double max_dist, Rcpp::NumericVector extent);
RcppExport SEXP _arrR_rcpp_update_coords(SEXP fishpopSEXP, SEXP iSEXP, SEXP move_distSEXP, SEXP max_distSEXP, SEXP extentSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< double >::type move_dist(move_distSEXP);
    Rcpp::traits::input_parameter< double >::type max_dist(max_distSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    rcpp_update_coords(fishpop, i, move_dist, max_dist, extent);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_arrR_rcpp_add_input", (DL_FUNC) &_arrR_rcpp_add_input, 3},
    {"_arrR_rcpp_cell_from_xy", (DL_FUNC) &_arrR_rcpp_cell_from_xy, 3},
    {"_arrR_rcpp_closest_reef", (DL_FUNC) &_arrR_rcpp_closest_reef, 2},
    {"_arrR_rcpp_convert_nutr", (DL_FUNC) &_arrR_rcpp_convert_nutr, 2},
    {"_arrR_rcpp_diffuse_values", (DL_FUNC) &_arrR_rcpp_diffuse_values, 5},
    {"_arrR_rcpp_dist_reef", (DL_FUNC) &_arrR_rcpp_dist_reef, 4},
    {"_arrR_rcpp_fishpop_growth", (DL_FUNC) &_arrR_rcpp_fishpop_growth, 14},
    {"_arrR_rcpp_get_bearing", (DL_FUNC) &_arrR_rcpp_get_bearing, 4},
    {"_arrR_rcpp_mineralization", (DL_FUNC) &_arrR_rcpp_mineralization, 3},
    {"_arrR_rcpp_modify_degree", (DL_FUNC) &_arrR_rcpp_modify_degree, 2},
    {"_arrR_rcpp_mortality", (DL_FUNC) &_arrR_rcpp_mortality, 8},
    {"_arrR_rcpp_move_behav", (DL_FUNC) &_arrR_rcpp_move_behav, 11},
    {"_arrR_rcpp_move_rand", (DL_FUNC) &_arrR_rcpp_move_rand, 9},
    {"_arrR_rcpp_nutr_uptake", (DL_FUNC) &_arrR_rcpp_nutr_uptake, 5},
    {"_arrR_rcpp_reincarnate", (DL_FUNC) &_arrR_rcpp_reincarnate, 9},
    {"_arrR_rcpp_remove_output", (DL_FUNC) &_arrR_rcpp_remove_output, 2},
    {"_arrR_rcpp_respiration", (DL_FUNC) &_arrR_rcpp_respiration, 8},
    {"_arrR_rcpp_rlognorm", (DL_FUNC) &_arrR_rcpp_rlognorm, 4},
    {"_arrR_rcpp_seagrass_growth", (DL_FUNC) &_arrR_rcpp_seagrass_growth, 16},
    {"_arrR_rcpp_shuffle", (DL_FUNC) &_arrR_rcpp_shuffle, 2},
    {"_arrR_rcpp_translate_torus", (DL_FUNC) &_arrR_rcpp_translate_torus, 2},
    {"_arrR_rcpp_update_coords", (DL_FUNC) &_arrR_rcpp_update_coords, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_arrR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
