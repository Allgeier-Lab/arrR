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
//' @param nutrients_loss,detritus_loss Double with fraction removed from each cell.
//'
//' @details
//' Simulates loss of nutrients (i.e., output of the system) for each cell and time
//' step. The loss is calculated as a ratio of the present nutrients in each cell.
//'
//' @references
//' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
//' Netherlands, Dordrecht. <https://doi.org/10.1007/978-94-011-2342-6>
//'
//' @return void
//'
//' @aliases rcpp_nutr_output
//' @rdname rcpp_nutr_output
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_nutr_output(Rcpp::NumericMatrix seafloor, double nutrients_loss, double detritus_loss) {

  // loop through all raster cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // calculate output amount
    double nutrients = seafloor(i, 4) * nutrients_loss;

    double detritus = seafloor(i, 5) * detritus_loss;

    // remove output from nutrients pool
    seafloor(i, 4) -= nutrients;

    // remove output from detritus pool
    seafloor(i, 5) -= detritus;

  }
}
