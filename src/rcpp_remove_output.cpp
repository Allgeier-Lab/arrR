#include "rcpp_remove_output.h"

//' rcpp_remove_output
//'
//' @description Rcpp remove nutrients
//'
//' @param seafloor Matrix with seafloor values.
//' @param nutrients_output Double with fraction removed from each cell.
//'
//' @details
//' Rcpp implementation to remove nutrient from raster cells.
//'
//' @return void
//'
//' @aliases rcpp_remove_output
//' @rdname rcpp_remove_output
//'
//' @export
// [[Rcpp::export]]
void rcpp_remove_output(Rcpp::NumericMatrix seafloor, double nutrients_output) {

  // loop through all raster cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // add nutrient input of timestep
    seafloor(i, 4) -= seafloor(i, 4) * nutrients_output;

  }
}

/*** R
# diffuse values and save result
rcpp_remove_output(seafloor = seafloor_values,
                   nutrients_output = parameters$nutrients_output)
*/
