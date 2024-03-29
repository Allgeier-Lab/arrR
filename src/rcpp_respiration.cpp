#include <Rcpp.h>

#include "rcpp_respiration.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

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
//' Function to simulate respiration of fish individuals based on movement, body +
//' size and water temperature. The respiration is temperature dependent with an
//' activity multiplier (Hanson et al. 1997). Originally descibed in Kitchell et al. (1977).
//'
//' If respiration is a infinite number (due to zero division), set to
//' \code{erespiration=1.0}.
//'
//' @references
//' Hanson, P.C., Johnson, T.B., Schindler, D.E., Kitchell, J.F., 1997. Fish
//' Bioenergetics 3.0 for Windows manual (Manual). University of Wisconsin-Madison,
//' Centre for Limnology, Madison,USA.
//'
//' Kitchell, J.F., Stewart, D.J., Weininger, D., 1977. Applications of a bioenergetics
//' model to Yellow Perch (Perca flavescens) and Walleye (Stizostedion vitreum vitreum).
//' J. Fish. Res. Bd. Can. 34, 1922–1935. <https://doi.org/10.1139/f77-258>
//'
//' @return void
//'
//' @aliases rcpp_respiration
//' @rdname rcpp_respiration
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_respiration(Rcpp::NumericMatrix fishpop,
                      double resp_intercept, double resp_slope,
                      double resp_temp_low, double resp_temp_max, double resp_temp_optm,
                      double water_temp, double min_per_i) {

  // scale intercept to correct tick scale from (g/g/day to min_per_i)
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

    // calculate respiration
    // oxycaloric coefficient c=13560.0 in J/gO2; energy-density of fish e=4800.0 J/g(wet weight)
    double respiration = (resp_intercept * std::pow(fishpop(i, 6), resp_slope) *
                          temp_dependence * fishpop(i, 7)) * 13560.0 / 4800.0;

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
