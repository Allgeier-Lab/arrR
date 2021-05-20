//#ifndef RCPP_UPDATE_COORDS
//#define RCPP_UPDATE_COORDS

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_update_coords(Rcpp::NumericMatrix fishpop, int i,
                        double move_dist, double max_dist, Rcpp::NumericVector extent) ;

//#endif // RCPP_UPDATE_COORDS
