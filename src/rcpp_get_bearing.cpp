#include <Rcpp.h>

#include "rcpp_get_bearing.h"

using namespace Rcpp;

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
// [[Rcpp::export]]
double rcpp_get_bearing(double x1, double y1, double x2, double y2) {

  // calculate bearing between fish coords and shortest reef cell
  double theta = atan2(y2 - y1, x2 - x1);

  // correct if in bottom left sector?
  if (theta < 0.0) {

    theta += 2 * M_PI;

  }

  // MH: Do I need to loop here?

  // convert to degree?
  theta = theta * (180.0 / M_PI);

  return(theta);
}

/*** R
x <- -12.5
y <-  23.5

reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0), ncol = 2, byrow = TRUE)

closest_reef <- rcpp_closest_reef(x = x, y = y, coords_reef = reef_matrix)

reef_id <- closest_reef[1] + 1

rcpp_get_bearing(x1 = x, y1 = y, x2 = reef_matrix[reef_id, 1], y2 = reef_matrix[reef_id, 2])
*/
