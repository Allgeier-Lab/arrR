//#ifndef RCPP_ALLOCATION_RATIO
//#define RCPP_ALLOCATION_RATIO

#include "Rcpp.h"

using namespace Rcpp;

double rcpp_allocation_ratio(double biomass, double biomass_min, double biomass_max,
                             double threshold, double slope);

//#endif // RCPP_ALLOCATION_RATIO
