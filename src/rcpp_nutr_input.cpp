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
//' @param nutr_input Vector with amount of nutrient input each timestep.
//'
//' @details
//' Simulate external nutrient input to the each cell. The \code{nutr_input}
//' vector must have as many elements as \code{max_i} to add input each timestep.
//' If not nutrients should be added, set all values of \code{nutr_input} to zero.
//'
//' @references
//' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
//' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
//'
//' @return void
//'
//' @aliases rcpp_nutr_input
//' @rdname rcpp_nutr_input
//'
// [[Rcpp::export]]
void rcpp_nutr_input(Rcpp::NumericMatrix seafloor, double nutr_input) {

  // loop through all raster cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // add nutrient input of timestep
    seafloor(i, 4) += nutr_input;

  }
}

/*** R
rcpp_nutr_input(seafloor = seafloor_values,nutr_input = nutr_input)
*/
