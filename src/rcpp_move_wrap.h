//#ifndef RCPP_MOVE_WRAP
//#define RCPP_MOVE_WRAP

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_move_wrap(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                    String movement, Rcpp::NumericVector pop_thres_reserves,
                    double move_mean, double move_var, double move_visibility,
                    double move_reef, double move_border, double move_return, double max_dist,
                    Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);

//#endif // RCPP_MOVE_WRAP