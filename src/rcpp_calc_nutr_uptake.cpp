#include "rcpp_calc_nutr_uptake.h"
#include "rcpp_convert_nutr.h"

//' rcpp_calc_nutr_uptake
//'
//' @description Rcpp calculate nutrient uptake
//'
//' @param nutrients,biomass Numeric with nutrient and biomass amount of cell.
//' @param v_max,k_m,time_frac Numeric with parameters
//'
//' @details
//' Rcpp implementation to calculate nutrient uptake of cell
//'
//' @return double
//'
//' @aliases rcpp_calc_nutr_uptake
//' @rdname rcpp_calc_nutr_uptake
//'
//' @keywords export
// [[Rcpp::export]]
double rcpp_calc_nutr_uptake(double nutrients, double biomass,
                             double v_max, double k_m, double time_frac) {

  // convert water column nutrients to umol/l
  double nutrients_umol = rcpp_convert_nutr(nutrients, "umol");

  // 1 x 1 x m = 1 cubic m = 1000l * 3m water depth
  // nutrients_umol = nutrients_umol / (1000 * 3);

  // calculate bg and ag uptake depending on nutrients and biomass
  double v_amb =  v_max * nutrients_umol / (k_m + nutrients_umol);

  // daily uptake
  double uptake_umol = v_amb * biomass * time_frac / 1000;

  // convert back to g
  double uptake_g = rcpp_convert_nutr(uptake_umol, "g");

  return(uptake_g);

}
