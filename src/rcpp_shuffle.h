//#ifndef RCPP_SHUFFLE
//#define RCPP_SHUFFLE

#include "Rcpp.h"
#include "random"
#include "chrono"

using namespace Rcpp;

Rcpp::IntegerVector rcpp_shuffle(int min, int max);

//#endif // RCPP_SHUFFLE
