#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_calc_respiration
//'
//' @description Rcpp calculate respration
//'
//' @param fishpop Matrix with fishpop values.
//' @param resp_intercept,resp_slope Numeric with parameters.
//' @param resp_temp_low,resp_temp_max,resp_temp_optm Numeric with parameters.
//' @param water_temp,min_per_i, Numeric with parameters.
//'
//' @details
//' Rcpp implementation to calculate respiration of fish individuals.
//'
//' @return void
//'
//' @aliases rcpp_calc_respiration
//' @rdname rcpp_calc_respiration
//'
//' @export
// [[Rcpp::export]]
void rcpp_calc_respiration(Rcpp::NumericMatrix fishpop,
                             double resp_intercept, double resp_slope,
                             double resp_temp_low, double resp_temp_max, double resp_temp_optm,
                             double water_temp, double min_per_i) {

  // scale intercept to correct tick scale
  resp_intercept = resp_intercept * (1.0 / 24.0) * (1.0 / 60.0) * min_per_i;

  // for f(T) temperature dependence function for respiration
  double v_resp = (resp_temp_max - water_temp) / (resp_temp_max - resp_temp_optm);

  double z_resp = log(resp_temp_low) * (resp_temp_max - resp_temp_optm);

  double y_resp = log(resp_temp_low) * (resp_temp_max - resp_temp_optm + 2);

  double x_resp = std::pow(z_resp, 2) * std::pow((1 + std::pow((1 + 40 / y_resp), 0.5)), 2) / 400.0;

  // this is the f(t) equation 2
  double temp_dependence = std::pow(v_resp, x_resp) * exp(x_resp * (1 - v_resp));

  // loop through all fish individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    // calculate respiration
    // Oxycaloric coefficient in J/gO2 consumed multiplied by the energy-density of fish
    // to result in unit of tick^-1
    double respiration = (resp_intercept * std::pow(fishpop(i, 6), resp_slope) *
                          temp_dependence * fishpop(i, 9)) * 13560 * (1.0 / 4800.0);

    // check if finite number
    bool check_finite = std::isfinite(respiration);

    // respiration is finite number; keep value
    if (check_finite) {

      // update respiration col
      fishpop(i, 10) = respiration;

    // respiration is infinite (divided by zero probably), use 1 instead
    } else {

      // update respiration col
      fishpop(i, 10) = 1;

    }
  }
}

/*** R
resp_intercept <- parameters$resp_intercept * (1 / 24) * (1 / 60) * min_per_i

rcpp_calc_respiration(fishpop = fishpop_values,
                      resp_intercept = parameters$resp_intercept,
                      resp_slope = parameters$resp_slope,
                      resp_temp_low = parameters$resp_temp_low,
                      resp_temp_optm = parameters$resp_temp_optm,
                      resp_temp_max = parameters$resp_temp_max,
                      water_temp = water_temp, min_per_i = min_per_i)
*/
