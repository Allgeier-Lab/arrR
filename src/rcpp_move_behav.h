//#ifndef RCPP_MOVE_FISHPOP
//#define RCPP_MOVE_FISHPOP

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_move_behav(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                     Rcpp::NumericVector pop_thres_reserves,
                     double move_mean, double move_var,
                     double move_reef, double move_border, double move_return, double max_dist,
                     Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);

//#endif // RCPP_MOVE_FISHPOP
