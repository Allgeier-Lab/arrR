// [[Rcpp::depends(RcppDist)]]

#include <Rcpp.h>
#include <chrono>
#include <random>

#include "rcpp_runif.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_runif
//'
//' @description
//' Rcpp runif.
//'
//' @param min,max Double boundaries.
//'
//' @details
//' Draws random number from uniform distribution.
//'
//' @references
//' <https://www.cplusplus.com/reference/random/uniform_real_distribution/>
//'
//' @return double
//'
//' @aliases rcpp_runif
//' @rdname rcpp_runif
//'
//' @keywords internal
// [[Rcpp::export]]
double rcpp_runif(double min, double max) {

  // obtain a time-based seed
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();

  std::mt19937 generator(seed);

  std::uniform_real_distribution<double> distribution(min, max);

  double x = distribution(generator);

  return x;

}

/*** R
lo <- 0
hi <- 360
n <- 1000000

rand_a <- purrr::map_dbl(1:n, function(i) rcpp_runif(min = lo, max = hi))

rand_b <- runif(n = n, min = lo, max = hi)

range(rand_a)
range(rand_b)

plot(density(rand_a), col = "#3C9BED", main = "Density", xlim = c(lo, hi))
lines(density(rand_b), col = "#EC579A")

abline(v = lo, lty = 2, col = "grey")
abline(v = hi, lty = 2, col = "grey")

bench::mark(
  rcpp_runif(min = lo, max = hi),
  runif(n = 1, min = lo, max = hi),
  check = FALSE, iterations = 1000000, relative = TRUE,
)
*/
