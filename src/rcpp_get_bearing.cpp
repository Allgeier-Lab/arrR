#include "rcpp_get_bearing.h"

//' rcpp_get_bearing
//'
//' @description Rcpp get bearing between two coordinates
//'
//' @param x_fish,y_fish Double with xy coords of fish individual.
//' @param x_reef,y_reef Double with xy coords of closest reef.
//'
//' @details
//' Rcpp implementation to get bearing between fish individual and closest reef cell.
//'
//' @return double
//'
//' @aliases rcpp_get_bearing
//' @rdname rcpp_get_bearing
//'
//' @keywords export
// [[Rcpp::export]]
double rcpp_get_bearing(double x_fish, double y_fish,
                        double x_reef, double y_reef) {

  // calculate bearing between fish coords and shortest reef cell
  double theta = atan2(y_reef - y_fish, x_reef - x_fish);

  // correct if in bottom left sector?
  if (theta < 0.0) {

    theta += 2 * M_PI;

  }

  // convert to degree?
  theta = theta * (180.0 / M_PI);

  return(theta);
}

/*** R
closest_reef <- rcpp_closest_reef(fishpop_values[3, c(3, 4), drop = FALSE], coords_reef)

reef_id <- closest_reef[1] + 1

rcpp_get_bearing(fishpop_values[3, "x"], fishpop_values[3, "y"],
                 coords_reef[reef_id, "x"], coords_reef[reef_id, "y"])
*/
