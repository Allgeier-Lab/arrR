#include <Rcpp.h>

#include "rcpp_quantile.h"

using namespace Rcpp;

//' rcpp_quantile
//'
//' @description
//' Rcpp quantile
//'
//' @param x NumericVector with values.
//' @param q Double with quantile.
//'
//' @details
//' Returns q-th percentile of vector.
//'
//' @references
//' Rcpp quantile implementation from
//' <https://stackoverflow.com/questions/26786078/rcpp-quantile-implementation>
//'
//' @return double
//'
//' @aliases rcpp_quantile
//' @rdname rcpp_quantile
//'
//' @keywords internal
// [[Rcpp::export]]
double rcpp_quantile(Rcpp::NumericVector x, double q) {

  // clone to avoid modify-in-place of x
  Rcpp::NumericVector x_sort = Rcpp::clone(x);

  // sort x from min to max
  std::sort(x_sort.begin(), x_sort.end());

  // get qth object
  double r = x_sort[x.size() * q];

  return r;
}

/*** R
x <- runif(n = 1000000)
rcpp_quantile(x = x, q = 0.95)
quantile(x = x, probs = 0.95)
*/
