//#ifndef RCPP_MORTALITY
//#define RCPP_MORTALITY

#include "Rcpp.h"
#include "rcpp_cell_from_xy.h"
#include "rcpp_shuffle.h"
#include "rcpp_reincarnate.h"

using namespace Rcpp;

void rcpp_mortality(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                    Rcpp::NumericMatrix seafloor,
                    double pop_linf, double pop_n_body, double pop_reserves_max,
                    Rcpp::NumericVector extent, Rcpp::NumericVector dimensions);

//#endif // RCPP_MORTALITY
