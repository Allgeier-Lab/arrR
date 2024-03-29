#include <Rcpp.h>

#include "rcpp_rlognorm.h"
#include "rcpp_rnorm.h"

using namespace Rcpp;

//' rcpp_rlognorm
//'
//' @description
//' Rcpp rlognorm.
//'
//' @param mean Double with mean value.
//' @param sd Double with sd value.
//' @param min,max Double boundaries of random number.
//'
//' @details
//' Draws random number from (truncated) log-norm distribution. Function uses log-transformed
//' values and a normal distribution internally.
//'
//' @return double
//'
//' @aliases rcpp_rlognorm
//' @rdname rcpp_rlognorm
//'
//' @keywords internal
// [[Rcpp::export]]
double rcpp_rlognorm(double mean, double sd, double min, double max) {

  double rand = 0.0;

  // check if values are within boundaries
  if ((mean < min) || (mean > max)) {

    Rcpp::stop("The 'mean' values is not within the 'min'/'max' boundaries (rcpp_rlognorm).");

  }

  // https://en.wikipedia.org/wiki/Log-normal_distribution
  double log_mean = std::log(std::pow(mean, 2) /
                             std::sqrt(std::pow(mean, 2) + std::pow(sd, 2)));

  double log_sd = std::sqrt(std::log(1 + (std::pow(sd, 2) / std::pow(mean, 2))));

  // get log norm random number
  rand = std::exp(rcpp_rnorm(log_mean, log_sd, std::log(min), std::log(max)));

  return (rand);
}

/*** R
mean <- 25
sd <- 10
n <- 100000
max <- 50

foo <- function(n, m, s) {
  # https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
  location <- log(m ^ 2 / sqrt(s ^ 2 + m ^ 2))
  shape <- sqrt(log(1 + (s ^ 2 / m ^ 2)))
  rlnorm(n = n, meanlog = location, sdlog = shape)
}

rand_a <- purrr::map_dbl(1:n, function(i) rcpp_rlognorm(mean = mean, sd = sd,
                                                         min = 0.0, max = Inf))

rand_b <- purrr::map_dbl(1:n, function(i) foo(n = 1, m = mean, s = sd))

max_value <- ceiling(max(c(rand_a, rand_b)))

mean(rand_a)
mean(rand_b)

sd(rand_a)
sd(rand_b)

plot(density(rand_a), col = "#3C9BED", main = "Density", xlim = c(0, max_value))
lines(density(rand_b), col = "#EC579A")

abline(v = mean, lty = 2, col = "grey")
abline(v = mean - sd, lty = 2, col = "grey")
abline(v = mean + sd, lty = 2, col = "grey")

bench::mark(
  rcpp_rlognorm(mean = mean, sd = sd, min = 0.0, max = Inf), foo(n = 1, m = mean, s = sd),
  check = FALSE, iterations = 1000000, relative = TRUE,
)
*/
