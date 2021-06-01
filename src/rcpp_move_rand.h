//#ifndef RCPP_MOVE_RAND
//#define RCPP_MOVE_RAND

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_move_rand(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                    double move_mean, double move_var, double move_visibility,
                    double max_dist, bool reef_attraction,
                    Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);

//#endif // RCPP_MOVE_RAND
