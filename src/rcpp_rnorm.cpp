// [[Rcpp::depends(RcppDist)]]

#include <Rcpp.h>
#include <truncnorm.h>

#include "rcpp_rnorm.h"

using namespace Rcpp;

//' rcpp_rlognorm
//'
//' @description
//' Rcpp rlognorm.
//'
//' @param mean Double with mean.
//' @param sd Double with sd
//' @param min,max Double boundaries.
//'
//' @details
//' Draws random number from norm distribution.
//'
//' @references
//' Truncated normal distribution from: J.B. Duck-Mayr (2018). RcppDist: 'Rcpp'
//' Integration of Additional Probability Distributions. R package version 0.1.1.
//' <https://CRAN.R-project.org/package=RcppDist>
//'
//' @return double
//'
//' @aliases rcpp_rlognorm
//' @rdname rcpp_rlognorm
//'
//' @keywords internal
// [[Rcpp::export]]
double rcpp_rnorm(double mean, double sd, double min, double max) {

  double rand = 0.0;

  // check if values are within boundaries
  if ((mean < min) || (mean > max)) {

    Rcpp::stop("The 'mean' values is not within the 'min'/'max' boundaries.");

  }

  if ((mean != 0) || (sd != 0)) {

    // https://github.com/duckmayr/RcppDist
    rand = r_truncnorm(mean, sd, min, max);

  }

  return (rand);
}

/*** R
mean <- 0.5
sd <- 0.25
n <- 1000000

rand_a <- purrr::map_dbl(1:n, function(i) rcpp_rnorm(mean = mean, sd = sd,
                                                     min = 0.0, max = 1.0))

rand_b <- purrr::map_dbl(1:n, function(i) rnorm(n = 1, mean = mean, sd = sd))

mean(rand_a)
mean(rand_b)

sd(rand_a)
sd(rand_b)

plot(density(rand_a), col = "#3C9BED", main = "Density", xlim = range(c(rand_a, rand_b)))
lines(density(rand_b), col = "#EC579A")

abline(v = mean, lty = 2, col = "grey")
abline(v = mean - sd, lty = 2, col = "grey")
abline(v = mean + sd, lty = 2, col = "grey")

bench::mark(
  rcpp_rlognorm(mean = mean, sd = sd, min = 0.0, max = Inf),
  rnorm(n = 1, mean = mean, sd = sd),
  check = FALSE, iterations = 1000000, relative = TRUE,
)
*/
