#include <Rcpp.h>

#include "rcpp_get_bearing.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_get_bearing
//'
//' @description
//' Rcpp get bearing between two coordinate pairs.
//'
//' @param x1,y1 Double with first xy coords pair.
//' @param x2,y2 Double with second xy coords pair.
//'
//' @details
//' Get bearing between (x1,y1) and (x2,y2).
//'
//' @return double
//'
//' @aliases rcpp_get_bearing
//' @rdname rcpp_get_bearing
//'
//' @keywords internal
// [[Rcpp::export(.rcpp_get_bearing)]]
double rcpp_get_bearing(double x1, double y1, double x2, double y2) {

  // calculate bearing between fish coords and shortest reef cell
  double theta = atan2(y2 - y1, x2 - x1);

  // correct if in bottom left sector?
  if (theta < 0.0) {

    theta += 2 * M_PI;

  }

  // convert to degree?
  theta = theta * (180.0 / M_PI);

  return(theta);
}

/*** R
closest_reef <- .rcpp_closest_reef(fishpop_values[3, "x"], fishpop_values[3, "y"],
                                   coords_reef)
reef_id <- closest_reef[1] + 1
.rcpp_get_bearing(fishpop_values[3, "x"], fishpop_values[3, "y"],
                  coords_reef[reef_id, "x"], coords_reef[reef_id, "y"])
*/
