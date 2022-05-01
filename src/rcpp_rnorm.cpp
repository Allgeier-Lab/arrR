#include <Rcpp.h>
#include <chrono>
#include <random>

#include "rcpp_rnorm.h"

using namespace Rcpp;

//' rcpp_rnorm
//'
//' @description
//' Rcpp random truncated norm
//'
//' @param mean Double with mean value.
//' @param sd Double with sd value.
//' @param min,max Double boundaries of random number.
//'
//' @details
//' Draws random number from (truncated) normal distribution using rejection
//' approach
//'
//' @return double
//'
//' @aliases rcpp_rnorm
//' @rdname rcpp_rnorm
//'
//' @keywords internal
// [[Rcpp::export]]
double rcpp_rnorm(double mean, double sd, double min, double max) {

  // init result double
  double rand = 0.0;

  // check if values are within boundaries
  if ((mean < min) || (mean > max)) {

    Rcpp::stop("The 'mean' values is not within the 'min'/'max' boundaries (rcpp_rnorm).");

  }

  // if mean == 0 and sd == 0; rand = 0
  if ((mean != 0) || (sd != 0)) {

    // obtain a time-based seed
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();

    // create random number generater
    std::mt19937 generator(seed);

    // init normal distribution
    std::normal_distribution<double> distribution(mean, sd);

    // draw random number
    rand = distribution(generator);

    // rejection-approach
    while (rand < min || rand > max) {

      // redraw number outside limits
      rand = distribution(generator);

    }
  }

  return (rand);
}

/*** R
mean <- 0.5
sd <- 0.25
n <- 100000
min = 0.0
max = 1.0

rand_a <- purrr::map_dbl(1:n, function(i) rcpp_rnorm(mean = mean, sd = sd,
                                                      min = min, max = max))

rand_b <- purrr::map_dbl(1:n, function(i) rnorm(n = 1, mean = mean, sd = sd))

mean(rand_a)
mean(rand_b)

sd(rand_a)
sd(rand_b)

plot(density(rand_a), col = "#3C9BED", main = "Density", xlim = range(c(rand_a, rand_b, rand_c)))
lines(density(rand_b), col = "#EC579A")

abline(v = mean, lty = 2, col = "grey")
abline(v = mean - sd, lty = 2, col = "grey")
abline(v = mean + sd, lty = 2, col = "grey")

bench::mark(
  rcpp_rnorm(mean = mean, sd = sd, min = 0.0, max = Inf), rnorm(n = 1, mean = mean, sd = sd),
  check = FALSE, iterations = 1000000, relative = TRUE,
)
*/
