//#ifndef RCPP_MOVE_FISHPOP
//#define RCPP_MOVE_FISHPOP

#include "Rcpp.h"
#include "rcpp_closest_reef.h"
#include "rcpp_get_bearing.h"
#include "rcpp_rlognorm.h"
#include "rcpp_translate_torus.h"
#include "rcpp_update_coords.h"

using namespace Rcpp;

void rcpp_move_behav(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                     Rcpp::NumericVector pop_reserves_thres,
                     double move_mean, double move_var,
                     double move_reef, double move_border, double move_return, double max_dist,
                     Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);

//#endif // RCPP_MOVE_FISHPOP
