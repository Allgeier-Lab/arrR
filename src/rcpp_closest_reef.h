//#ifndef RCPP_CLOSEST_REEF
//#define RCPP_CLOSEST_REEF

#include "Rcpp.h"

using namespace Rcpp;

Rcpp::NumericVector rcpp_closest_reef(Rcpp::NumericVector coords_temp,
                                      Rcpp::NumericMatrix coords_reef);

//#endif // RCPP_CLOSEST_REEF
