#include <Rcpp.h>

#include "rcpp_get_reef.h"

using namespace Rcpp;

// [[Rcpp::interfaces(cpp)]]

//' rcpp_get_reef
//'
//' @description
//' Rcpp get reef matrix
//'
//' @param seafloor Matrix with seafloor values.
//'
//' @details
//' Get matrix with cell id and coordinates of reef cells.
//'
//' @return matrix
//'
//' @aliases rcpp_get_reef
//' @rdname rcpp_get_reef
//'
//' @keywords internal
// [[Rcpp::export(.rcpp_get_reef)]]
Rcpp::NumericMatrix rcpp_get_reef(Rcpp::NumericMatrix seafloor) {

  // get vector of reef id
  Rcpp::NumericVector reef_id = seafloor(_, 15);

  // count all reef cells
  int reef_n = std::count(reef_id.begin(), reef_id.end(), 1);

  // init matrix with 3 columns and reef_n rows
  NumericMatrix reef(reef_n, 3);

  // init row counter
  int counter = 0;

  for (int i = 0; i < seafloor.nrow(); i++) {

    if (seafloor(i, 15) == 1) {

      // get row id
      reef(counter, 0) = i;

      // get x,y coords
      reef(counter, 1) = seafloor(i, 0);
      reef(counter, 2) = seafloor(i, 1);

      // increase counter
      counter++;

    }
  }

  return reef;

}

/*** R
# convert seafloor and fishpop as matrix
seafloor_values <- as.matrix(terra::as.data.frame(input_seafloor,
                                                  xy = TRUE, na.rm = FALSE))

foo <- function(seafloor) {

  # get cell id of reef
  cells_reef <- which(seafloor[, "reef"] == 1)

  # get cell id of reef cells and coordinates of reef cells
  matrix(data = c(cells_reef, seafloor[cells_reef, c("x", "y")]),
         ncol = 3)
}

bench::mark(
  foo(seafloor_values),
  .rcpp_get_reef(seafloor_values), iterations = 1000, relative = TRUE,
  )
*/
