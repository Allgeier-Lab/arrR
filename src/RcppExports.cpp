// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/arrR.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_allocation_ratio
double rcpp_allocation_ratio(double biomass, double biomass_min, double biomass_max, double threshold, double slope);
RcppExport SEXP _arrR_rcpp_allocation_ratio(SEXP biomassSEXP, SEXP biomass_minSEXP, SEXP biomass_maxSEXP, SEXP thresholdSEXP, SEXP slopeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type biomass(biomassSEXP);
    Rcpp::traits::input_parameter< double >::type biomass_min(biomass_minSEXP);
    Rcpp::traits::input_parameter< double >::type biomass_max(biomass_maxSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< double >::type slope(slopeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_allocation_ratio(biomass, biomass_min, biomass_max, threshold, slope));
    return rcpp_result_gen;
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
// rcpp_simulate
void rcpp_simulate(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix fishpop, Rcpp::NumericVector nutrients_input, Rcpp::List seafloor_track, Rcpp::List fishpop_track, Rcpp::List parameters, Rcpp::String movement, Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions, int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in, bool verbose);
RcppExport SEXP _arrR_rcpp_simulate(SEXP seafloorSEXP, SEXP fishpopSEXP, SEXP nutrients_inputSEXP, SEXP seafloor_trackSEXP, SEXP fishpop_trackSEXP, SEXP parametersSEXP, SEXP movementSEXP, SEXP extentSEXP, SEXP dimensionsSEXP, SEXP max_iSEXP, SEXP min_per_iSEXP, SEXP save_eachSEXP, SEXP seagrass_eachSEXP, SEXP burn_inSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type seafloor(seafloorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type fishpop(fishpopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type nutrients_input(nutrients_inputSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type seafloor_track(seafloor_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type fishpop_track(fishpop_trackSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type parameters(parametersSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type movement(movementSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type extent(extentSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dimensions(dimensionsSEXP);
    Rcpp::traits::input_parameter< int >::type max_i(max_iSEXP);
    Rcpp::traits::input_parameter< int >::type min_per_i(min_per_iSEXP);
    Rcpp::traits::input_parameter< int >::type save_each(save_eachSEXP);
    Rcpp::traits::input_parameter< int >::type seagrass_each(seagrass_eachSEXP);
    Rcpp::traits::input_parameter< int >::type burn_in(burn_inSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_simulate(seafloor, fishpop, nutrients_input, seafloor_track, fishpop_track, parameters, movement, extent, dimensions, max_i, min_per_i, save_each, seagrass_each, burn_in, verbose);
    return R_NilValue;
END_RCPP
}

// validate (ensure exported C++ functions exist before calling them)
static int _arrR_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _arrR_RcppExport_registerCCallable() { 
    R_RegisterCCallable("arrR", "_arrR_RcppExport_validate", (DL_FUNC)_arrR_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_arrR_rcpp_allocation_ratio", (DL_FUNC) &_arrR_rcpp_allocation_ratio, 5},
    {"_arrR_rcpp_rlognorm", (DL_FUNC) &_arrR_rcpp_rlognorm, 4},
    {"_arrR_rcpp_simulate", (DL_FUNC) &_arrR_rcpp_simulate, 15},
    {"_arrR_RcppExport_registerCCallable", (DL_FUNC) &_arrR_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_arrR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
