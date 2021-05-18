#include "rcpp_shuffle.h"

// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_shuffle(int min, int max) {

  IntegerVector x = seq(min, max);

  std::random_shuffle(x.begin(), x.end());

  return x;
}
