//#ifndef RCPP_MOVE_RAND
//#define RCPP_MOVE_RAND

#include "Rcpp.h"
#include "rcpp_closest_reef.h"
#include "rcpp_modify_degree.h"
#include "rcpp_rlognorm.h"
#include "rcpp_translate_torus.h"
#include "rcpp_update_coords.h"

using namespace Rcpp;

void rcpp_move_rand(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                    double move_mean, double move_var, double move_visibility,
                    double max_dist, bool reef_attraction,
                    Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);

//#endif // RCPP_MOVE_RAND
