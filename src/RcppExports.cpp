// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_calc_dist_reef
Rcpp::NumericMatrix rcpp_calc_dist_reef(Rcpp::NumericMatrix fish_population, Rcpp::NumericMatrix coords_reef);
RcppExport SEXP _coRal_rcpp_calc_dist_reef(SEXP fish_populationSEXP, SEXP coords_reefSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fish_population(fish_populationSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type coords_reef(coords_reefSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calc_dist_reef(fish_population, coords_reef));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_coRal_rcpp_calc_dist_reef", (DL_FUNC) &_coRal_rcpp_calc_dist_reef, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_coRal(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}