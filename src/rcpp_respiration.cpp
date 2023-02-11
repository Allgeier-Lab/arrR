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
//' @param resp_intercept,resp_slope Vector with regression parameters.
//' @param resp_temp_low,resp_temp_max,resp_temp_optm Vector with water temperature parameters.
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
                      Rcpp::NumericVector resp_intercept, Rcpp::NumericVector resp_slope,
                      Rcpp::NumericVector resp_temp_low, Rcpp::NumericVector resp_temp_max,
                      Rcpp::NumericVector resp_temp_optm,
                      double water_temp, double min_per_i) {

  // loop through all fish individuals
  for (int i = 0; i < fishpop.nrow(); i++) {
    Rcout << "(51resp)resp intercept vector = "<< resp_intercept[0] << " " << resp_intercept[1] << std::endl;
    // get current species id
    int species_temp = fishpop(i, 1) - 1;

    // scale intercept to correct tick scale from (g/g/day to min_per_i)
    resp_intercept[species_temp] = resp_intercept[species_temp] / (24.0 * 60.0) * min_per_i;
    Rcout << "(57resp)resp intercept vector = "<< resp_intercept[0] << " " << resp_intercept[1] << std::endl;
    if(i == 3) {Rcpp:stop("testing this");}// for f(T) temperature dependence function for respiration
    double v_resp = (resp_temp_max[species_temp] - water_temp) /
      (resp_temp_max[species_temp] - resp_temp_optm[species_temp]);

    double z_resp = log(resp_temp_low[species_temp]) *
      (resp_temp_max[species_temp] - resp_temp_optm[species_temp]);

    double y_resp = log(resp_temp_low[species_temp]) *
      (resp_temp_max[species_temp] - resp_temp_optm[species_temp] + 2);

    double x_resp = std::pow(z_resp, 2) * std::pow((1 + std::pow((1 + 40 / y_resp), 0.5)), 2) / 400.0;

    // this is the f(t) equation 2
    double temp_dependence = std::pow(v_resp, x_resp) * exp(x_resp * (1 - v_resp));

    // calculate respiration
    // oxycaloric coefficient c=13560.0 in J/gO2; energy-density of fish e=4800.0 J/g(wet weight)
    double respiration = (resp_intercept[species_temp] * std::pow(fishpop(i, 7), resp_slope[species_temp]) *
                          temp_dependence * fishpop(i, 8)) * 13560.0 / 4800.0;
    Rcout << "(76 resp) temp dependence = " << temp_dependence <<  " fishpop(i, 7) = " << fishpop(i, 7) << " fishpop(i, 8) = " << fishpop(i, 8) << " resp_intercept[species_temp] = " << resp_intercept[species_temp]<< " resp_slope = " << resp_slope[species_temp]<<std::endl;
    Rcout << "(77resp) respiration at " << i << " = " << respiration << std::endl;
    // check if finite number
    bool check_finite = std::isfinite(respiration);

    // respiration is finite number; keep value
    if (check_finite) {
      Rcout << "(83resp) fishpop(i, 9) before adding respiration = " << fishpop(i, 9) << std::endl;
      // update respiration col
      fishpop(i, 9) = respiration;

    // respiration is infinite (divided by zero probably), use 1 instead
    } else {

      // update respiration col
      fishpop(i, 9) = 1.0;

    }
    resp_intercept[species_temp] = resp_intercept[species_temp] * (24.0 * 60.0) / min_per_i;

  }
}
