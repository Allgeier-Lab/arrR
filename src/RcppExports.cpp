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
// rcpp_diffuse_values
Rcpp::NumericMatrix rcpp_diffuse_values(Rcpp::NumericMatrix seafloor_values, Rcpp::NumericMatrix cell_adj, double nutrients_diffusion, double detritus_diffusion, double detritus_dead_diffusion);
RcppExport SEXP _arrR_rcpp_diffuse_values(SEXP seafloor_valuesSEXP, SEXP cell_adjSEXP, SEXP nutrients_diffusionSEXP, SEXP detritus_diffusionSEXP, SEXP detritus_dead_diffusionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor_values(seafloor_valuesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cell_adj(cell_adjSEXP);
    Rcpp::traits::input_parameter< double >::type nutrients_diffusion(nutrients_diffusionSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_diffusion(detritus_diffusionSEXP);
    Rcpp::traits::input_parameter< double >::type detritus_dead_diffusion(detritus_dead_diffusionSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_diffuse_values(seafloor_values, cell_adj, nutrients_diffusion, detritus_diffusion, detritus_dead_diffusion));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_arrR_rcpp_calc_dist_reef", (DL_FUNC) &_arrR_rcpp_calc_dist_reef, 4},
    {"_arrR_rcpp_diffuse_values", (DL_FUNC) &_arrR_rcpp_diffuse_values, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_arrR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
