#include <Rcpp.h>
#include <chrono>
#include <random>

#include "rcpp_shuffle.h"

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_shuffle
//'
//' @description
//' Rcpp shuffle vector.
//'
//' @param x NumericVector with elements to shuffle.
//'
//' @details
//' Creates vector with IDs from \code{min} to \code{max} in random order.
//'
//' @references
//' How to use time-based seed taken from <http://www.cplusplus.com/reference/algorithm/shuffle/>
//'
//' @return vector
//'
//' @aliases rcpp_shuffle
//' @rdname rcpp_shuffle
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_shuffle(Rcpp::NumericVector x) {

  // clone to avoid modify-in-place
  Rcpp::NumericVector x_shuffle = Rcpp::clone(x);

  // obtain a time-based seed
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();

  // shuffle vector; std::default_random_engine(seed)
  std::shuffle(x_shuffle.begin(), x_shuffle.end(), std::mt19937(seed));

  return x_shuffle;
}

/*** R
rcpp_shuffle(seq(from = 1, to = 10, by = 1))
*/
