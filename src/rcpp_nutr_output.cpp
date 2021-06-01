#include "rcpp_nutr_output.h"

//' rcpp_nutr_output
//'
//' @description Rcpp nutrient output
//'
//' @param seafloor Matrix with seafloor values.
//' @param nutrients_output Double with fraction removed from each cell.
//'
//' @details
//' Simulate loss of nutrients to to output of the system for each cell and timestep.
//'
//' @references
//' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
//' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
//'
//' @return void
//'
//' @aliases rcpp_nutr_output
//' @rdname rcpp_nutr_output
//'
//' @export
// [[Rcpp::export]]
void rcpp_nutr_output(Rcpp::NumericMatrix seafloor, double nutrients_output) {

  // loop through all raster cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // add nutrient input of timestep
    seafloor(i, 4) -= seafloor(i, 4) * nutrients_output;

  }
}

/*** R
# diffuse values and save result
rcpp_nutr_output(seafloor = seafloor_values,
                   nutrients_output = parameters$nutrients_output)
*/
