#include "rcpp_rlognorm.h"

//' rcpp_rlognorm
//'
//' @description Create random number from log distribution
//'
//' @param n Integer with amount of numbers.
//' @param mean Double with mean.
//' @param sd Double with sd
//' @param min,max Double boundaries.
//'
//' @details
//' Get random number from log-norm distribution. Function uses log-transformed
//' values and a normal distribution internally.
//'
//' @references
//' Truncated normal distribution from: J.B. Duck-Mayr (2018). RcppDist: 'Rcpp'
//' Integration of Additional Probability Distributions. R package version 0.1.1.
//' https://CRAN.R-project.org/package=RcppDist
//'
//' @return double
//'
//' @aliases rcpp_rlognorm
//' @rdname rcpp_rlognorm
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_rlognorm(int n, double mean, double sd, double min, double max) {

  // https://en.wikipedia.org/wiki/Log-normal_distribution

  double log_mean = std::log(std::pow(mean, 2) / std::sqrt(std::pow(mean, 2) + std::pow(sd, 2)));

  double log_sd = std::sqrt(std::log(1 + (std::pow(sd, 2) / std::pow(mean, 2))));

  Rcpp::NumericVector log_rand = rtruncnorm(n, log_mean, log_sd,
                                            log(min), log(max));

  for (int i = 0; i < log_rand.length(); i++) {

    log_rand(i) = std::exp(log_rand(i));

  }

  return (log_rand);
}

/*** R

mean <- 5
sd <- 2
n <- 100000

foo <- function(n, m, s) {
  # https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  rlnorm(n = n, location, shape)
}

rand_a <- rcpp_rlognorm(n = n, mean = mean, sd = sd, min = 0, max = 10)
rand_b <- purrr::map_dbl(1:n, function(i) foo(n = 1, m = mean, s = sd))
rand_c <- arrR::rlognorm(n = n, mean = mean, sd = sd)

max_value <- ceiling(max(c(rand_a, rand_b, rand_c)))

mean(rand_a)
mean(rand_b)
mean(rand_c)

plot(density(rand_a), col = "#3C9BED", main = "Density", xlim = c(0, max_value))
lines(density(rand_b), col = "#EC579A")
lines(density(rand_c), col = "#A1C721")

abline(v = mean, lty = 2)
abline(v = mean - sd, lty = 2)
abline(v = mean + sd, lty = 2)
*/
