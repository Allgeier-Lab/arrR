#include <Rcpp.h>

#include "rcpp_nutr_input.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_nutr_input
//'
//' @description
//' Rcpp simulate nutrient input.
//'
//' @param seafloor Matrix with seafloor values.
//' @param nutrients_input Vector with amount of nutrient input each time step.
//'
//' @details
//' Simulate external nutrient input to the each cell. The \code{nutrients_input}
//' vector must have as many elements as \code{max_i} to add input each time step.
//'
//' If no nutrients should be added, set all values of \code{nutrients_input} to
//' zero.
//'
//' @references
//' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
//' Netherlands, Dordrecht. <https://doi.org/10.1007/978-94-011-2342-6>
//'
//' @return void
//'
//' @aliases rcpp_nutr_input
//' @rdname rcpp_nutr_input
//'
//' @keywords internal
// [[Rcpp::export(.rcpp_nutr_input)]]
void rcpp_nutr_input(Rcpp::NumericMatrix seafloor, double nutrients_input) {

  // loop through all raster cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // add nutrient input of time step
    seafloor(i, 4) += nutrients_input;

  }
}
