#include <Rcpp.h>
#include "rcpp_nutr_output.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_nutr_output
//'
//' @description
//' Rcpp nutrient output.
//'
//' @param seafloor Matrix with seafloor values.
//' @param nutrients_output,detritus_output Double with fraction removed from each cell.
//'
//' @details
//' Simulates loss of nutrients (i.e., output of the system) for each cell and timestep.
//' The loss is calculated as a ratio of the present nutrients in each cell.
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
void rcpp_nutr_output(Rcpp::NumericMatrix seafloor, double nutrients_output, double detritus_output) {

  // loop through all raster cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // calculate output amount
    double nutrients = seafloor(i, 4) * nutrients_output;

    double detritus = seafloor(i, 5) * detritus_output;

    // add nutrient input of timestep
    seafloor(i, 4) -= nutrients;

    // add nutrient input of timestep
    seafloor(i, 5) -= detritus;

  }
}

/*** R
# diffuse values and save result
rcpp_nutr_output(seafloor = seafloor_values, nutrients_output = parameters$nutrients_output,
                 detritus_output = parameters$detritus_output)
*/
