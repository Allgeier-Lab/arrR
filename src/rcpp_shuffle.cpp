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
//' @param elements Logical if vector elements or iteratos are returned.
//'
//' @details
//' Shuffles the element of a vector (or the elements iterators).
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
Rcpp::NumericVector rcpp_shuffle(Rcpp::NumericVector x, bool elements) {

  // obtain a time-based seed
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();

  // init shuffle vector
  Rcpp::NumericVector x_shuffle (x.length());

  // return shuffled vector elements
  if (elements) {

    // clone to avoid modify-in-place
    x_shuffle = Rcpp::clone(x);

  // return shuffled iterators
  } else {

    // loop through all elements of vector
    for (int i = 0; i < x_shuffle.length(); i++) {

      // create sequence of iterators
      x_shuffle(i) = i + 1;

    }
  }

  // shuffle vector; std::default_random_engine(seed)
  std::shuffle(x_shuffle.begin(), x_shuffle.end(), std::mt19937(seed));

  return x_shuffle;

}

/*** R
x <- seq(from = 11, to = 18, by = 1)

rcpp_shuffle(x = x, elements = TRUE)
rcpp_shuffle(x = x, elements = FALSE)
*/
