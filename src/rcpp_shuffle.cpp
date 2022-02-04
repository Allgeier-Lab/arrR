#include <Rcpp.h>
#include <random>
#include <chrono>

#include "rcpp_shuffle.h"

using namespace Rcpp;

//' rcpp_shuffle
//'
//' @description
//' Rcpp shuffle vector.
//'
//' @param min,max Integer with minimum and maximum value of vector.
//'
//' @details
//' Creates vector with IDs from \code{min} to \code{max}, but in random order.
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
Rcpp::IntegerVector rcpp_shuffle(int min, int max) {

  // create sequance from min to max
  Rcpp::IntegerVector x = Rcpp::seq(min, max);

  // obtain a time-based seed
  int seed = std::chrono::system_clock::now().time_since_epoch().count();

  // shuffle vector; std::default_random_engine(seed)
  std::shuffle(x.begin(), x.end(), std::mt19937(seed));

  return x;
}

/*** R
rcpp_shuffle(min = 1, max = 10)
*/
