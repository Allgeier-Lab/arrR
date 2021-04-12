//#ifndef RCPP_GET_BEARING
//#define RCPP_GET_BEARING

#include "Rcpp.h"

using namespace Rcpp;

double rcpp_get_bearing(double x_fish, double y_fish,
                        double x_reef, double y_reef);

//#endif // RCPP_GET_BEARING
