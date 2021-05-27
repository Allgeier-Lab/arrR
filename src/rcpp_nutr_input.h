//#ifndef RCPP_NUTR_INPUT
//#define RCPP_NUTR_INPUT

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_nutr_input(Rcpp::NumericMatrix seafloor, Rcpp::NumericVector nutr_input,
                     int timestep);

//#endif // RCPP_NUTR_INPUT
