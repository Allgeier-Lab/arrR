#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_calc_mineralization
//'
//' @description Add describtion
//'
//' @param seafloor Add describtion
//' @param detritus_dead_decomp,detritus_mineralization seafloor Add describtion
//'
//' @details
//' Add describtion
//'
//' @return Add describtion
//'
//' @aliases rcpp_calc_mineralization
//' @rdname rcpp_calc_mineralization
//'
//' @keywords export
// [[Rcpp::export]]
void rcpp_calc_mineralization(Rcpp::NumericMatrix seafloor,
                                double detritus_dead_decomp,
                                double detritus_mineralization) {

  for (int i = 0; i < seafloor.nrow(); i++) {

  // calculate decomposition amount
  double dead_decompostion = seafloor(i, 6) * detritus_dead_decomp;

  // redistribute dead detritus to active detritus
  seafloor(i, 5) += dead_decompostion;

  seafloor(i, 6) -= dead_decompostion;

  // get detritus amount that goes into nutrients pool
  double mineralization = seafloor(i, 5) * detritus_mineralization;

  // add to nutrients pool
  seafloor(i, 4) += mineralization;

  // remove from detritus pool
  seafloor(i, 5) -= mineralization;

  }
}
