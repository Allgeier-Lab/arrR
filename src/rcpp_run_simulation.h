//#ifndef RCPP_RUN_SIMULATION
//#define RCPP_RUN_SIMULATION

#include "Rcpp.h"

// [[Rcpp::depends(RcppProgress)]]

using namespace Rcpp;

void rcpp_run_simulation(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix fishpop,
                         List seafloor_track, List fishpop_track, List parameters,
                         int pop_n, String movement, double max_dist, Rcpp::NumericVector pop_reserves_thres,
                         Rcpp::NumericMatrix coords_reef, Rcpp::NumericMatrix cell_adj,
                         Rcpp::NumericVector extent, Rcpp::NumericVector dimensions,
                         Rcpp::NumericVector nutr_input,
                         int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in,
                         bool verbose);

//#endif // RCPP_RUN_SIMULATION
