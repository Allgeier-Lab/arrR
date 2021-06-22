#include "rcpp_respiration.h"

//' rcpp_respiration
//'
//' @description
//' Rcpp simulate respration.
//'
//' @param fishpop Matrix with fishpop values.
//' @param resp_intercept,resp_slope Numeric with regression parameters.
//' @param resp_temp_low,resp_temp_max,resp_temp_optm Numeric with water temperature parameters.
//' @param water_temp,min_per_i Numeric with various parameters.
//'
//' @details
//' Function to simulate respiration of fish population individuals based on movement,
//' body size and water temperature. The respiration is temperature dependent with an
//' activity multiplier (Hanson et al. 1997). Originally descibed in Kitchell et al. (1977).
//'
//' If respiration is a infinite number (due to zero division), set to respiration = 1.0.
//'
//' @references
//' Hanson, P.C., Johnson, T.B., Schindler, D.E., Kitchell, J.F., 1997. Fish
//' Bioenergetics 3.0 for Windows manual (Manual). University of Wisconsin-Madison,
//' Centre for Limnology, Madison,USA.
//'
//' Kitchell, J.F., Stewart, D.J., Weininger, D., 1977. Applications of a bioenergetics
//' model to Yellow Perch (Perca flavescens) and Walleye (Stizostedion vitreum vitreum).
//' J. Fish. Res. Bd. Can. 34, 1922–1935. https://doi.org/10.1139/f77-258
//'
//' @return void
//'
//' @aliases rcpp_respiration
//' @rdname rcpp_respiration
//'
//' @export
// [[Rcpp::export]]
void rcpp_respiration(Rcpp::NumericMatrix fishpop,
                      double resp_intercept, double resp_slope,
                      double resp_temp_low, double resp_temp_max, double resp_temp_optm,
                      double water_temp, double min_per_i) {

  // scale intercept to correct tick scale
  resp_intercept = resp_intercept / (24.0 * 60.0) * min_per_i;

  // for f(T) temperature dependence function for respiration
  double v_resp = (resp_temp_max - water_temp) / (resp_temp_max - resp_temp_optm);

  double z_resp = log(resp_temp_low) * (resp_temp_max - resp_temp_optm);

  double y_resp = log(resp_temp_low) * (resp_temp_max - resp_temp_optm + 2);

  double x_resp = std::pow(z_resp, 2) * std::pow((1 + std::pow((1 + 40 / y_resp), 0.5)), 2) / 400.0;

  // this is the f(t) equation 2
  double temp_dependence = std::pow(v_resp, x_resp) * exp(x_resp * (1 - v_resp));

  // loop through all fish individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    // MH:  Make sure this is calculated using min_per_i
    // calculate respiration
    // Oxycaloric coefficient in J/gO2 consumed multiplied by the energy-density of fish
    // to result in unit of tick^-1
    double respiration = (resp_intercept * std::pow(fishpop(i, 6), resp_slope) *
                          temp_dependence * fishpop(i, 7)) * 13560.0 * (1.0 / 4800.0);

    // check if finite number
    bool check_finite = std::isfinite(respiration);

    // respiration is finite number; keep value
    if (check_finite) {

      // update respiration col
      fishpop(i, 8) = respiration;

    // respiration is infinite (divided by zero probably), use 1 instead
    } else {

      // update respiration col
      fishpop(i, 8) = 1.0;

    }
  }
}

/*** R
rcpp_respiration(fishpop = fishpop_values, resp_intercept = parameters$resp_intercept,
                 resp_slope = parameters$resp_slope, resp_temp_low = parameters$resp_temp_low,
                 resp_temp_optm = parameters$resp_temp_optm, resp_temp_max = parameters$resp_temp_max,
                 water_temp = water_temp, min_per_i = min_per_i)
*/
