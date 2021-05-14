//#ifndef RCPP_ADD_INPUT
//#define RCPP_ADD_INPUT

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_add_input(Rcpp::NumericMatrix seafloor, Rcpp::NumericVector nutr_input,
                    int timestep);

//#endif // RCPP_ADD_INPUT
