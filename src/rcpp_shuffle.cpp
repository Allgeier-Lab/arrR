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
//' @param min,max Integer with min/max values.
//'
//' @details
//' Creates vector with IDs from \code{min} to \code{max} in random order.
//'
//' @references
//' How to use time-based seed taken from
//' <http://www.cplusplus.com/reference/algorithm/shuffle/>
//'
//' @return vector
//'
//' @aliases rcpp_shuffle
//' @rdname rcpp_shuffle
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_shuffle(int min, int max) {

  // create sequence from min to max
  Rcpp::IntegerVector rand = Rcpp::seq(min, max);

  // obtain a time-based seed
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();

  // shuffle vector; std::default_random_engine(seed)
  std::shuffle(rand.begin(), rand.end(), std::mt19937(seed));

  return rand;
}

/*** R
rcpp_shuffle(min = 1, max = 10)
*/
