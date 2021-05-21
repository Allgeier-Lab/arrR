#include "rcpp_allocation_ratio.h"

//' rcpp_allocation_ratio
//'
//' @description Rcpp allocation ratio
//'
//' @param biomass Numeric with biomass.
//' @param biomass_min,biomass_max Numeric with minum and maximum of biomass.
//' @param threshold,slope Numeric with function parameters.
//'
//' @details
//' Rcpp implementation of the allocation ratio
//'
//' @return void
//'
//' @aliases rcpp_allocation_ratio
//' @rdname rcpp_allocation_ratio
//'
//' @export
// [[Rcpp::export]]
double rcpp_allocation_ratio(double biomass, double biomass_min, double biomass_max,
                             double threshold, double slope) {

  // init ratio
  double ratio = 0.0;

  // calculate threshold biomass
  double threshold_temp = biomass_min + (biomass_max - biomass_min) * threshold;

  // set to 1 if below threshold
  if (biomass < threshold_temp) {

    ratio = 1.0;

  // calculate ratio if above threshold
  } else {

    // calc turning point of allocation function
    double midpoint = -log(2.0) / log(threshold);

    // calculate temp minimum at which sigmoid starts
    double min_temp = biomass_min + (biomass_max - biomass_min) * threshold;

    // normalize biomass cell to 0 - 1
    double biomass_norm = (biomass - min_temp) / (biomass_max - min_temp);

    // calculate ratio0
    ratio = 1 - (1 / (1 + std::pow((std::pow(biomass_norm, midpoint) /
      (1 - std::pow(biomass_norm, midpoint))), -slope)));

  }

  return ratio;

}

/*** R
parameters$seagrass_thres <- 1/4
parameters$seagrass_slope <- 1.5

biomass <- parameters$bg_biomass_min +
(parameters$bg_biomass_max - parameters$bg_biomass_min) * 1/4

rcpp_allocation_ratio(biomass, parameters$bg_biomass_min, parameters$bg_biomass_max,
                      parameters$seagrass_thres, parameters$seagrass_slope)
*/
