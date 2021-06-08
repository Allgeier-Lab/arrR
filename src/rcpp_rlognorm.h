//#ifndef RCPP_RLOGNORM
//#define RCPP_RLOGNORM

#include "Rcpp.h"

// [[Rcpp::depends(RcppDist)]]

using namespace Rcpp;

double rcpp_rlognorm(double mean, double sd, double min, double max);

//#endif // RCPP_RLOGNORM
