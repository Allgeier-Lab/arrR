//#ifndef RCPP_RLOGNORM
//#define RCPP_RLOGNORM

#include "Rcpp.h"
#include "truncnorm.h"
// [[Rcpp::depends(RcppDist)]]

using namespace Rcpp;

Rcpp::NumericVector rcpp_rlognorm(int n, double mean, double sd,
                                  double min, double max);

//#endif // RCPP_RLOGNORM
