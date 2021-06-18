//#ifndef RCPP_REINCARNATE
//#define RCPP_REINCARNATE

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_reincarnate(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, int fish_id,
                      Rcpp::NumericMatrix seafloor, Rcpp::NumericVector extent, Rcpp::NumericVector dimensions,
                      double pop_linf, double pop_n_body, double pop_max_reserves,
                      String reason);

//#endif // RCPP_REINCARNATE
