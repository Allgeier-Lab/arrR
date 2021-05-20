#include "rcpp_shuffle.h"

//' rcpp_shuffle
//'
//' @description Create random id vector
//'
//' @param min,max Integer with minum and maximum value of vector.
//'
//' @details
//' Rcpp implementation to get vector with random ids.
//'
//' @return vector
//'
//' @aliases rcpp_shuffle
//' @rdname rcpp_shuffle
//'
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_shuffle(int min, int max) {

  IntegerVector x = seq(min, max);

  std::random_shuffle(x.begin(), x.end());

  return x;
}
