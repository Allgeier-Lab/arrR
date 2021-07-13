//#ifndef RCPP_FISHPOP_GROWTH
//#define RCPP_FISHPOP_GROWTH

#include "Rcpp.h"
#include "rcpp_cell_from_xy.h"
#include "rcpp_reincarnate.h"
#include "rcpp_shuffle.h"

using namespace Rcpp;

void rcpp_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                         Rcpp::NumericMatrix seafloor,
                         double pop_k, double pop_linf, double pop_a, double pop_b,
                         double pop_n_body, double pop_reserves_max, double pop_reserves_consumption,
                         Rcpp::NumericVector extent, Rcpp::NumericVector dimensions,
                         double min_per_i);

//#endif // RCPP_FISHPOP_GROWTH
