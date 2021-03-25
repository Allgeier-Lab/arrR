//#ifndef RCPP_MOVE_FISHPOP
//#define RCPP_MOVE_FISHPOP

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_move_fishpop(Rcpp::NumericMatrix fishpop,
                       Rcpp::NumericVector move_dist, double pop_mean_move,
                       Rcpp::NumericVector extent, Rcpp::NumericVector dimensions,
                       double pop_visibility, bool reef_attraction);

//#endif // RCPP_MOVE_FISHPOP
