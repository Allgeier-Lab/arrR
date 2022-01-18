#include <Rcpp.h>
#include "rcpp_sum.h"

using namespace Rcpp;

//' rcpp_sum
//'
//' @description
//' Rcpp vector sum
//'
//' @param x Numeric vector.
//'
//' @details
//' Calculate sum of vector
//'
//' @references
//' https://teuder.github.io/rcpp4everyone_en/030_basic_usage.html
//'
//' @return double
//'
//' @aliases rcpp_sum
//' @rdname rcpp_sum
//'
//' @export
// [[Rcpp::export]]
double rcpp_sum(Rcpp::NumericVector x) {

  // init result double
  double sum = 0.0;

  // loop through vector
  for(int i = 0; i < x.length(); ++i){

    sum += x[i];

  }
  return(sum);
}

/*** R
# rcpp_sum
x <- runif(n = 10000, min = 0, max = 100)

sum(x)
rcpp_sum(x)

bench::mark(sum(x), rcpp_sum(x), iterations = 100, relative = TRUE)
*/
