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
//' Rcpp implementation of the allocation ratio. If the threshold is positive, all
//' the ratio is 1 if biomass is below the threshold. If threshold is negative, the
//' ratio will be 0.5 at the threshold.
//'
//' @references
//' User wmsmith on CrossValidated: "Is there a formula for an s-shaped curve with
//' domain and range [0,1]?" \link{https://stats.stackexchange.com/questions/214877/}
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

  // init threshold temp
  double threshold_temp = 0.0;

  // if threshold is bigger than zero, all allocation before threshold goes to bg
  if (threshold > 0) {

    threshold_temp = biomass_min + (biomass_max - biomass_min) * threshold;

  // set to value biomass can not be smaller
  } else {

    threshold_temp = -1.0;

  }

  // set to 100% if below threshold; never true if threshold < 0
  if (biomass < threshold_temp) {

    ratio = 1.0;

  // calculate ratio if above threshold
  } else {

    // start sigmoid at treshold if theshold < 0
    if (threshold > 0) {

      // calculate temp minimum at which sigmoid starts
      biomass_min = biomass_min + (biomass_max - biomass_min) * threshold;

    }

    // normalize biomass cell to 0 - 1
    double biomass_norm = (biomass - biomass_min) / (biomass_max - biomass_min);

    // calc turning point of allocation function
    double midpoint = -log(2.0) / log(std::abs(threshold));

    // calculate ratio0
    ratio = 1 - (1 / (1 + std::pow((std::pow(biomass_norm, midpoint) /
      (1 - std::pow(biomass_norm, midpoint))), -slope)));

  }

  return ratio;
}

/*** R
parameters <- arrR::default_parameters

parameters$seagrass_thres <- 1/4
parameters$seagrass_slope <- 3

biomass <- parameters$bg_biomass_min +
(parameters$bg_biomass_max - parameters$bg_biomass_min) * 1/4

rcpp_allocation_ratio(biomass, parameters$bg_biomass_min, parameters$bg_biomass_max,
                      parameters$seagrass_thres, parameters$seagrass_slope)

plot_allocation(parameters)
*/
