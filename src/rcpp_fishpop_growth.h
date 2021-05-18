//#ifndef RCPP_FISHPOP_GROWTH
//#define RCPP_FISHPOP_GROWTH

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                         Rcpp::NumericMatrix seafloor,
                         double pop_k, double pop_linf, double pop_a, double pop_b,
                         double pop_n_body, double pop_want_reserves, double pop_max_reserves,
                         double pop_consumption_prop,
                         Rcpp::NumericVector extent, Rcpp::NumericVector dimensions,
                         double min_per_i);

//#endif // RCPP_FISHPOP_GROWTH
