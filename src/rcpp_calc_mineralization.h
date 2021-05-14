//#ifndef RCPP_CALC_MINERALIZATION
//#define RCPP_CALC_MINERALIZATION

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_calc_mineralization(Rcpp::NumericMatrix seafloor,
                              double detritus_fish_ratio, double detritus_mineralization);

//#endif // RCPP_CALC_MINERALIZATION
