#include "rcpp_translate_torus.h"

//' rcpp_translate_torus
//'
//' @description
//' Rcpp translate coordinates around torus.
//'
//' @param coords Vector with coordinates.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//'
//' @details
//' Torus translation of coordinates if they exceed the provided extent. The translation
//' is done until coordinate is within extent (i.e., could be translated several times
//' if the difference is big).
//'
//' @return vector
//'
//' @aliases rcpp_translate_torus
//' @rdname rcpp_translate_torus
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_translate_torus(Rcpp::NumericVector coords,
                                         Rcpp::NumericVector extent) {

  // translate x coords left side
  while (coords(0) < extent(0)) {

    coords(0) = extent(1) - (extent(0) - coords(0));

  }

  // translate x coord right side
  while (coords(0) > extent(1)) {

    coords(0) = extent(0) + (coords(0) - extent(1));

  }

  // translate y coord bottom
  while (coords(1) < extent(2)) {

    coords(1) = extent(3) - (extent(2) - coords(1));

  }

  // translate y coord top
  while (coords(1) > extent(3)) {

    coords(1) = extent(2) + (coords(1) - extent(3));

  }

  return(coords);
}

/*** R
# rcpp_translate_torus
rcpp_translate_torus(coords = c(5, -5.5), extent = c(-10, 10, -10, 10))

rcpp_translate_torus(coords = c(12.5, -5.5), extent = c(-10, 10, -10, 10))
*/
