#include "rcpp_rlognorm.h"

//' rcpp_rlognorm
//'
//' @description Create random number from log distribution
//'
//' @param mean Double with mean.
//' @param sd Double with sd
//'
//' @details
//' Get random number from log-norm distribution. Function uses log-transformed
//' values and a normal distribution internally.
//'
//' @return double
//'
//' @aliases rcpp_rlognorm
//' @rdname rcpp_rlognorm
//'
//' @keywords export
// [[Rcpp::export]]
double rcpp_rlognorm(double mean, double sd) {

  // https://en.wikipedia.org/wiki/Log-normal_distribution
  double log_mean = std::log(std::pow(mean, 2) / std::sqrt(std::pow(mean, 2) + std::pow(sd, 2)));

  double log_sd = std::sqrt(std::log(1 + (std::pow(sd, 2) / std::pow(mean, 2))));

  double log_rand = std::exp(Rcpp::rnorm(1, log_mean, log_sd)(0));

  return (log_rand);
}

// //' @keywords export
// // [[Rcpp::export]]
// double rcpp_rlognorm(double mean, double sd) {
//
//   // double log_mean = std::log(mean);
//   //
//   // double log_sd = std::log(sd);
//
//   double log_rand = Rcpp::rlnorm(1, std::log(mean), std::log(sd))(0);
//
//   return (log_rand);
// }

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

rand_a <- purrr::map_dbl(1:n, function(i) rcpp_rlognorm(mean = mean, sd = sd))
rand_b <- rlnorm(n = n, meanlog = log(mean), sdlog = log(sd))
rand_c <- purrr::map_dbl(1:n, function(i) foo(n = 1, m = mean, s = sd))

max_value <- ceiling(max(c(rand_a, rand_b, rand_c)))
breaks <- seq(from = 0, to = max_value, by = 1)

mean(rand_a)
mean(rand_b)
mean(rand_c)

plot(density(rand_a), col = "#3C9BED", main = "Density")
lines(density(rand_b), col = "#EC579A")
lines(density(rand_c), col = "#A1C721")

abline(v = mean, lty = 2)
abline(v = mean - sd, lty = 2)
abline(v = mean + sd, lty = 2)
*/
