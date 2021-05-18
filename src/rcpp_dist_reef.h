//#ifndef RCPP_DIST_REEF
//#define RCPP_DIST_REEF

#include "Rcpp.h"

using namespace Rcpp;

Rcpp::NumericVector rcpp_dist_reef(Rcpp::NumericMatrix seafloor,
                                   Rcpp::NumericMatrix coords_reef,
                                   Rcpp::NumericVector extent,
                                   bool torus);

//#endif // RCPP_DIST_REEF
