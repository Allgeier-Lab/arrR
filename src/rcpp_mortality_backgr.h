//#ifndef RCPP_MORTALITY_BACKGR
//#define RCPP_MORTALITY_BACKGR

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_mortality_backgr(Rcpp::NumericMatrix fishpop,
                           Rcpp::NumericMatrix fishpop_track,
                           Rcpp::NumericMatrix seafloor,
                           Rcpp::NumericVector fish_id,
                           Rcpp::NumericVector cell_id,
                           double pop_linf,
                           double pop_n_body,
                           double pop_want_reserves);

//#endif // RCPP_MORTALITY_BACKGR
