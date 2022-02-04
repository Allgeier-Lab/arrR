#include <Rcpp.h>

#include "rcpp_get_max_dist.h"

#include "rcpp_quantile.h"
#include "rcpp_rlognorm.h"

using namespace Rcpp;

// [[Rcpp::interfaces(cpp)]]

// rcpp_get_max_dist
//
// @description
// Rcpp get maximum movement distance
//
// @param movement String specifing movement algorithm. Either 'rand', 'attr' or 'behav'.
// @param parameters List with parameters.
// @param n_rand Integer with amount of random numbers.
//
// @details
// Calculate double with maximum movement distance. The distance is the 95% quantile
// of n_rand random numbers.
//
// @return double
//
// @aliases rcpp_get_max_dist
// @rdname rcpp_get_max_dist
//
// @keywords internal
double rcpp_get_max_dist(Rcpp::String movement, Rcpp::List parameters, int n_rand) {

  double max_dist = 0.0;

  // init temp parameter values for behav or rand/attr movement
  double mean_temp, var_temp;

  // create temp vector with 1 million values
  Rcpp::NumericVector max_dist_temp (n_rand);

  // get movement parameters depending on behavior
  if (movement == "behav") {

    mean_temp = parameters["move_return"];
    var_temp = 1.0;



  } else {

    mean_temp = parameters["move_mean"];
    var_temp = parameters["move_var"];

  }

  // create 1 mio random movement distances
  for (int i = 0; i < max_dist_temp.length(); i ++) {

    max_dist_temp[i] = rcpp_rlognorm(mean_temp, var_temp, 0.0, R_PosInf);

  }

  // get 95% precentile
  max_dist = rcpp_quantile(max_dist_temp, 0.95);

  return(max_dist);

}

/*** R
.rcpp_get_max_dist(movement = "rand", parameters = arrR_parameters, 1000000)
*/
