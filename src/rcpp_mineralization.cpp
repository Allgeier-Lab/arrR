#include "rcpp_mineralization.h"

//' rcpp_mineralization
//'
//' @description Rcpp mineralization
//'
//' @param seafloor Matrix with seafloor values.
//' @param detritus_fish_decomp,detritus_mineralization seafloor Numeric with parameters.
//'
//' @details
//' Function to redistribute fish detritus pool to overall detritus pool and decomposition.
//'
//' @references
//' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
//' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
//'
//' @return void
//'
//' @aliases rcpp_mineralization
//' @rdname rcpp_mineralization
//'
//' @export
// [[Rcpp::export]]
void rcpp_mineralization(Rcpp::NumericMatrix seafloor,
                         double detritus_mineralization, double detritus_fish_decomp) {

  // loop through all seafloor values
  for (int i = 0; i < seafloor.nrow(); i++) {

    // get detritus amount that goes into nutrients pool
    double mineralization = seafloor(i, 5) * detritus_mineralization;

    // calculate decomposition amount
    double fish_decompostion = seafloor(i, 6) * detritus_fish_decomp;

    // add detritus to nutrients pool
    seafloor(i, 4) += mineralization;

    // remove biomass from detritus pool
    seafloor(i, 5) -= mineralization;

    // redistribute fish detritus to biomass detritus
    seafloor(i, 5) += fish_decompostion;

    seafloor(i, 6) -= fish_decompostion;

  }
}

/*** R
cpp_mineralization(seafloor = seafloor_values,
                   detritus_mineralization = parameters$detritus_mineralization,
                   detritus_fish_decomp = parameters$detritus_mineralization)
*/
