//#ifndef RCPP_CALC_MORTALITY_BACKGROUND
//#define RCPP_CALC_MORTALITY_BACKGROUND

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_calc_mortality_background(Rcpp::NumericMatrix fishpop,
                                    Rcpp::NumericMatrix fishpop_track,
                                    Rcpp::NumericMatrix seafloor,
                                    Rcpp::NumericVector fish_id,
                                    Rcpp::NumericVector cell_id,
                                    double pop_linf,
                                    double pop_n_body,
                                    double pop_want_reserves);

//#endif // RCPP_CALC_MORTALITY_BACKGROUND
