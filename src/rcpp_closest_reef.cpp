#include <Rcpp.h>
#include "rcpp_closest_reef.h"

using namespace Rcpp;

//' rcpp_closest_reef
//'
//' @description
//' Rcpp get closest reef.
//'
//' @param x,y Numeric with xy coords of current individual.
//' @param coords_reef Matrix with ID and coords of reef cells.
//'
//' @details
//' Get ID and distance to closet reef cell. The first element of the returning
//' vector is the ID, the second the distance in meter.
//'
//' @return vector
//'
//' @aliases rcpp_closest_reef
//' @rdname rcpp_closest_reef
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_closest_reef(double x, double y, Rcpp::NumericMatrix coords_reef) {

  // init vector to store result
  Rcpp::NumericVector result (2, 0.0);

  // init reef_dist_temp which is shortest distance to reef
  double reef_dist_temp = R_PosInf;

  // init id of closest reef cell
  int reef_dist_id = NA_REAL;

  // calculate distance between current coords and closest reef cell
  for (int i = 0; i < coords_reef.nrow(); i++) {

    // calculate distance in x direction
    double dist_x = x - coords_reef(i, 1);

    // calculate distance in y direction
    double dist_y = y - coords_reef(i, 2);

    // calculate final distance
    double dist_xy = std::sqrt(std::pow(dist_x, 2.0) + std::pow(dist_y, 2.0));

    // check if current distance is smaller
    if (dist_xy < reef_dist_temp) {

      // update shortest distance
      reef_dist_temp = dist_xy;

      // update id
      reef_dist_id = i;

    }
  }

  // save results
  result[0] = reef_dist_id;

  result[1] = reef_dist_temp;

  return(result);
}

/*** R
rcpp_closest_reef(fishpop_values[1, "x"], fishpop_values[1, "y"], coords_reef)
rcpp_closest_reef(fishpop_values[3, "x"], fishpop_values[3, "y"], coords_reef)
*/
