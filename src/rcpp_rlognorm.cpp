// [[Rcpp::depends(RcppDist)]]

#include <Rcpp.h>
#include <truncnorm.h>

#include "rcpp_rlognorm.h"

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
//' Draws random number from log-norm distribution. Function uses log-transformed
//' values and a normal distribution internally.
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
//' @export
// [[Rcpp::export]]
double rcpp_rlognorm(double mean, double sd, double min, double max) {

  // https://en.wikipedia.org/wiki/Log-normal_distribution
  double log_mean = std::log(std::pow(mean, 2) /
                             std::sqrt(std::pow(mean, 2) + std::pow(sd, 2)));

  double log_sd = std::sqrt(std::log(1 + (std::pow(sd, 2) / std::pow(mean, 2))));

  // https://github.com/duckmayr/RcppDist
  double log_rand = std::exp(r_truncnorm(log_mean, log_sd, std::log(min), std::log(max)));

  return (log_rand);
}

/*** R
mean <- 5
sd <- 2
n <- 1000000
max <- 10

foo <- function(n, m, s) {
  # https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
  location <- log(m ^ 2 / sqrt(s ^ 2 + m ^ 2))
  shape <- sqrt(log(1 + (s ^ 2 / m ^ 2)))
  rlnorm(n = n, location, shape)
}

rand_a <- purrr::map_dbl(1:n, function(i) rcpp_rlognorm(mean = mean, sd = sd,
                                                        min = 0.0, max = max))

rand_b <- purrr::map_dbl(1:n, function(i) foo(n = 1, m = mean, s = sd))

max_value <- ceiling(max(c(rand_a, rand_b)))

mean(rand_a)
mean(rand_b)

plot(density(rand_a), col = "#3C9BED", main = "Density", xlim = c(0, max_value))
lines(density(rand_b), col = "#EC579A")

abline(v = mean, lty = 2, col = "grey")
abline(v = mean - sd, lty = 2, col = "grey")
abline(v = mean + sd, lty = 2, col = "grey")

bench::mark(
  rcpp_rlognorm(mean = mean, sd = sd, min = 0.0, max = Inf),
  foo(n = 1, m = mean, s = sd),
  check = FALSE, iterations = 1000000, relative = TRUE,
)
*/
