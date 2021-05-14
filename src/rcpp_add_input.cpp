#include "rcpp_add_input.h"

//' rcpp_add_input
//'
//' @description Rcpp add nutrient input
//'
//' @param seafloor Matrix with seafloor values.
//' @param nutr_input Vector with amount of nutrient input each timestep.
//' @param timestep Integer with current timestep.
//'
//' @details
//' Rcpp implementation to add nutrient input to raster cells.
//'
//' @return void
//'
//' @aliases rcpp_add_input
//' @rdname rcpp_add_input
//'
//' @export
// [[Rcpp::export]]
void rcpp_add_input(Rcpp::NumericMatrix seafloor, Rcpp::NumericVector nutr_input,
                    int timestep) {

  // use C++ indexing
  int timestep_temp = timestep - 1;

  // loop through all raster cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // add nutrient input of timestep
    seafloor(i, 4) += nutr_input(timestep_temp);

  }
}

/*** R
rcpp_add_input(seafloor = seafloor_values,
               nutr_input = nutr_input,
               timestep = timestep)
*/
