//#ifndef RCPP_CALC_FISHPOP_GROWTH
//#define RCPP_CALC_FISHPOP_GROWTH

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_calc_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                              Rcpp::NumericMatrix seafloor,
                              Rcpp::NumericVector fish_id, Rcpp::NumericVector cell_id,
                              Rcpp::NumericVector pop_thres_reserves,
                              double pop_k, double pop_linf,
                              double pop_a, double pop_b,
                              double pop_n_body, double pop_max_reserves, double pop_want_reserves,
                              double pop_consumption_prop,
                              double min_per_i);

//#endif // RCPP_CALC_FISHPOP_GROWTH
