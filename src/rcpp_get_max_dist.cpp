#include <Rcpp.h>

#include "rcpp_get_max_dist.h"
#include "rcpp_quantile.h"
#include "rcpp_rlognorm.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_get_max_dist
//'
//' @description
//' Rcpp get maximum movement distance
//'
//' @param movement String specifing movement algorithm.
//' @param parameters List with parameters.
//' @param n_rand Integer with amount of random numbers.
//'
//' @details
//' Calculate double with maximum movement distance. The distance is the 95% quantile
//' of \code{n_rand} random numbers.
//'
//' \code{movement} can be either 'rand', 'attr' or 'behav'.
//'
//' @return double
//'
//' @aliases rcpp_get_max_dist
//' @rdname rcpp_get_max_dist
//'
//' @keywords internal
// [[Rcpp::export]]
double rcpp_get_max_dist(std::string movement, Rcpp::List parameters, int n_rand) {

  // init temp parameter values for behav or rand/attr movement
  double mean_temp = 0.0, sd_temp = 0.0;

  // create temp vector with 1 million values
  Rcpp::NumericVector max_dist_temp (n_rand, 0.0);

  // get movement parameters depending on behavior
  if (movement == "behav") {

    // use either distance to closest_reef or move_return distance
    mean_temp = std::max(as<double>(parameters["move_return"]),
                         as<double>(parameters["move_mean"]));

    mean_temp = std::max(mean_temp, as<double>(parameters["move_reef"]));

    sd_temp = std::max(as<double>(parameters["move_sd"]), 1.0);

  } else {

    mean_temp = parameters["move_mean"];

    sd_temp = parameters["move_sd"];

  }

  // create 1 mio random movement distances
  for (int i = 0; i < max_dist_temp.length(); i ++) {

    max_dist_temp[i] = rcpp_rlognorm(mean_temp, sd_temp, 0.0, R_PosInf);

  }

  // get 95% precentile
  double max_dist = rcpp_quantile(max_dist_temp, 0.95);

  return(max_dist);
}

/*** R
rcpp_get_max_dist(movement = "attr", parameters = default_parameters, 1000000)
rcpp_get_max_dist(movement = "behav", parameters = default_parameters, 1000000)
*/
