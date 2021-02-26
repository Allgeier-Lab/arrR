#include "rcpp_translate_torus.h"

//' rcpp_translate_torus
//'
//' @description Rcpp translate torus
//'
//' @param coords Vector with coordinates.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//'
//' @details
//' Rcpp implementation to translate coordinates if they exceed extent.
//' "KSM" notes from Katrina to help understand code
//' "Q" questions Katrina has for Max
//' "C" code to add
//' @return void
//'
//' @aliases rcpp_translate_torus
//' @rdname rcpp_translate_torus
//'
//' @keywords export
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
rcpp_translate_torus(coords = c(5, -5.5),
                     extent = c(-10, 10, -10, 10))

rcpp_translate_torus(coords = c(12.5, -5.5),
                     extent = c(-10, 10, -10, 10))

*/
