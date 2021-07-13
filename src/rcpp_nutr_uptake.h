//#ifndef RCPP_NUTR_UPTAKE
//#define RCPP_NUTR_UPTAKE

#include "Rcpp.h"
#include "rcpp_convert_nutr.h"

using namespace Rcpp;

double rcpp_nutr_uptake(double nutrients, double biomass,
                        double v_max, double k_m, double time_frac);

//#endif // RCPP_NUTR_UPTAKE
