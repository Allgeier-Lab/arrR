#include <Rcpp.h>
#include "rcpp_translate_torus.h"

using namespace Rcpp;

//' rcpp_translate_torus
//'
//' @description
//' Rcpp translate coordinates around torus.
//'
//' @param x,y Double with x,y coordinates
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
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_translate_torus(double x, double y, Rcpp::NumericVector extent) {

  // init numeric vector
  Rcpp::NumericVector coords (2, 0.0);

  // check if x needs to be changed
  if (x < extent[0] || x > extent[1]) {

    // translate x coords left side
    while (x < extent[0]) {

      x = extent[1] - (extent[0] - x);

    }

    // translate x coord right side
    while (x > extent[1]) {

      x = extent[0] + (x - extent[1]);

    }
  }

  // check if y needs to be changed
  if (y < extent[2] || y > extent[3]) {

    // translate y coord bottom
    while (y < extent[2]) {

      y = extent[3] - (extent[2] - y);

    }

    // translate y coord top
    while (y > extent[3]) {

      y = extent[2] + (y - extent[3]);

    }
  }

  // save coords
  coords[0] = x; coords[1] = y;

  return(coords);
}

/*** R
# rcpp_translate_torus
rcpp_translate_torus(x = 5, y =  -5.5, extent = c(-10, 10, -10, 10))

rcpp_translate_torus(x = 12.5, y = -5.5, extent = c(-10, 10, -10, 10))
*/
