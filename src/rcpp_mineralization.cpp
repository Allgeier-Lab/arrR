#include <Rcpp.h>

#include "rcpp_mineralization.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_mineralization
//'
//' @description
//' Rcpp simulate mineralization.
//'
//' @param seafloor Matrix with seafloor values.
//' @param detritus_fish_decomp,detritus_mineralization seafloor Numeric with parameters.
//'
//' @details
//' Simulate mineralization of the detritus pool i.e., a ratio of the detritus pool
//' is added to the nutrients pool. The corresponding amount is removed from the
//' detritus pool.
//'
//' Also, simulates decomposition of the detritus fish_pool by removing a ratio and
//' adding it to the detritus pool.
//'
//' @references
//' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
//' Netherlands, Dordrecht. <https://doi.org/10.1007/978-94-011-2342-6>
//'
//' @return void
//'
//' @aliases rcpp_mineralization
//' @rdname rcpp_mineralization
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_mineralization(Rcpp::NumericMatrix seafloor,
                         double detritus_mineralization, double detritus_fish_decomp) {

  // loop through all seafloor values
  for (int i = 0; i < seafloor.nrow(); i++) {

    // get detritus amount that goes into nutrients pool
    double mineralization = seafloor(i, 5) * detritus_mineralization;

    // calculate decomposition amount
    double fish_decompostion = seafloor(i, 6) * detritus_fish_decomp;
    if (seafloor(i, 4) < 0) { Rcpp::stop("(48.) seafloor(i, 4) is neg");}
    // add detritus to nutrients pool
    seafloor(i, 4) += mineralization;
    if (seafloor(i, 4) < 0) { Rcpp::stop("(50m) seafloor(i, 4) is neg");}
    // remove biomass from detritus pool
    seafloor(i, 5) -= mineralization;

    // redistribute fish detritus to biomass detritus
    seafloor(i, 5) += fish_decompostion;

    seafloor(i, 6) -= fish_decompostion;
    Rcout << "(58m) seafloor(i,4) at " << i << " = " << seafloor(i, 4) << std::endl;
  }
}
