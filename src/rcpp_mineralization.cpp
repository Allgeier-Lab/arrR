#include "rcpp_mineralization.h"

//' rcpp_mineralization
//'
//' @description Rcpp mineralization
//'
//' @param seafloor Matrix with seafloor values.
//' @param detritus_fish_ratio,detritus_mineralization seafloor Numeric with parameters.
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
                              double detritus_fish_ratio, double detritus_mineralization) {

  // loop through all seafloor values
  for (int i = 0; i < seafloor.nrow(); i++) {

    // calculate decomposition amount
    double fish_decompostion = seafloor(i, 6) * detritus_fish_ratio;

    // redistribute fish detritus to active detritus
    seafloor(i, 5) += fish_decompostion;

    seafloor(i, 6) -= fish_decompostion;

    // get detritus amount that goes into nutrients pool
    double mineralization = seafloor(i, 5) * detritus_mineralization;

    // add to nutrients pool
    seafloor(i, 4) += mineralization;

    // remove from detritus pool
    seafloor(i, 5) -= mineralization;

  }
}
