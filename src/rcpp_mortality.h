//#ifndef RCPP_MORTALITY
//#define RCPP_MORTALITY

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_mortality(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                    Rcpp::NumericMatrix seafloor,
                    double pop_linf, double pop_n_body, double pop_want_reserves,
                    Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);

//#endif // RCPP_MORTALITY
