//#ifndef RCPP_RLOGNORM
//#define RCPP_RLOGNORM

// [[Rcpp::depends(RcppDist)]]

#include "Rcpp.h"
#include "truncnorm.h"

using namespace Rcpp;

double rcpp_rlognorm(double mean, double sd, double min, double max);

//#endif // RCPP_RLOGNORM
