//#ifndef RCPP_REINCARNATE
//#define RCPP_REINCARNATE

#include "Rcpp.h"

using namespace Rcpp;

void rcpp_reincarnate(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                      Rcpp::NumericMatrix seafloor,
                      int fish_id, int cell_id,
                      double pop_linf, double pop_n_body, double pop_want_reserves,
                      String reason);

//#endif // RCPP_REINCARNATE
