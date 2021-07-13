#include "rcpp_shuffle.h"

//' rcpp_shuffle
//'
//' @description
//' Rcpp shuffle vector.
//'
//' @param min,max Integer with minum and maximum value of vector.
//'
//' @details
//' Creates vector with IDs from \code{min} to \code{max}, but in random order.
//'
//' @return vector
//'
//' @aliases rcpp_shuffle
//' @rdname rcpp_shuffle
//'
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_shuffle(int min, int max) {

  // create sequance from min to max
  IntegerVector x = seq(min, max);

  // obtain a time-based seed
  // http://www.cplusplus.com/reference/algorithm/shuffle/
  int seed = std::chrono::system_clock::now().time_since_epoch().count();

  // shuffle vector; std::default_random_engine(seed)
  std::shuffle(x.begin(), x.end(), std::mt19937(seed));

  return x;
}

/*** R
rcpp_shuffle(min = 1, max = 10)
*/
