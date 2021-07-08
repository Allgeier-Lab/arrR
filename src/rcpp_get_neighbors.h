//#ifndef RCPP_GET_NEIGHBORS
//#define RCPP_GET_NEIGHBORS

#include "Rcpp.h"

using namespace Rcpp;

Rcpp::IntegerVector rcpp_get_neighbors(int id, int n_cell,  int n_col, int directions);

//#endif // RCPP_GET_NEIGHBORS
