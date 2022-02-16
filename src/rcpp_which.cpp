#include <Rcpp.h>
#include "rcpp_which.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_which
//'
//' @description
//' Rcpp which.
//'
//' @param x Integer with value to find.
//' @param y Vector with values.
//'
//' @details
//' Returns the index of \code{x} within the \code{y} vector.
//' The index of the first element is 0. If element is not present within \code{y}
//' \code{NA} is returned. Returns only the first element if \code{x} is present
//' multiple times in \code{y}.
//'
//' @return vector
//'
//' @aliases rcpp_which
//' @rdname rcpp_which
//'
//' @keywords internal
// [[Rcpp::export(.rcpp_which)]]
int rcpp_which(double x, Rcpp::NumericVector y) {

  // init iterator
  Rcpp::NumericVector::iterator itr;

  // init position
  int position;

  // find current y element in x
  itr = std::find(y.begin(), y.end(), x);

  // y was present in x
  if (itr != y.end()) {

    // calculate position of iterator
    position = (itr - y.begin());

  // y was not present
  } else {

    // set to NA
    position = NA_INTEGER;

  }

  return position;
}

/*** R
.rcpp_which(x = 10, y = c(2, 1, 10, 1, 3))
.rcpp_which(x = 3, y = c(2, 1, 10, 1, 3))
.rcpp_which(x = 11, y = c(2, 10, 1, 3))
*/
