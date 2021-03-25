//#ifndef RCPP_CELL_FROM_XY
//#define RCPP_CELL_FROM_XY

#include "Rcpp.h"

using namespace Rcpp;

int rcpp_cell_from_xy(Rcpp::NumericVector coords,
                      Rcpp::NumericVector dimensions,
                      Rcpp::NumericVector extent);

//#endif // RCPP_CELL_FROM_XY
