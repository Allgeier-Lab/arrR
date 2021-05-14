//#ifndef RCPP_RESPIRATION
//#define RCPP_RESPIRATION

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_respiration(Rcpp::NumericMatrix fishpop,
                      double resp_intercept, double resp_slope,
                      double resp_temp_low, double resp_temp_max, double resp_temp_optm,
                      double water_temp, double min_per_i);

//#endif // RCPP_RESPIRATION
