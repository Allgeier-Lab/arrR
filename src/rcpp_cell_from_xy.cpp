#include <Rcpp.h>

#include "rcpp_cell_from_xy.h"

using namespace Rcpp;

//' rcpp_cell_from_xy
//'
//' @description
//' Rcpp get cell from xy
//'
//' @param x,y Numeric with x,y coordinates.
//' @param extent Vector with extent (xmin, xmax, ymin, ymax).
//' @param dimensions Vector with number or rows and cols
//' @param rcpp Logical if TRUE Rcpp index is returned.
//'
//' @details
//' Get cell ID from xy coordinates. Allows only one coordinate pair at a time.
//' If \code{rcpp = TRUE} indexing starts at 0 in accordance with C++.
//'
//' @references
//' Code adapted from Robert J. Hijmans (2020). raster: Geographic Data Analysis
//' and Modeling. R package version 3.4-5. <https://CRAN.R-project.org/package=raster>
//'
//' @return int
//'
//' @aliases rcpp_cell_from_xy
//' @rdname rcpp_cell_from_xy
//'
//' @keywords internal
// [[Rcpp::export]]
int rcpp_cell_from_xy(double x, double y, Rcpp::NumericVector extent,
                      Rcpp::IntegerVector dimensions, bool rcpp) {

  // init cell id
  int cell_id = NA_REAL;

  // coords outside extent; return NA
  if (x < extent[0] || x > extent[1] || y < extent[2] || y > extent[3]) {

    return cell_id;

  // coords within extent
  } else {

    // calculates resolution
    // double grain_x = dimensions[1] / (extent[1] - extent[0]);
    double grain_x = dimensions[1] / (extent[1] - extent[0]);

    double grain_y = dimensions[0]/ (extent[3] - extent[2]);

    if (grain_x != grain_y) {

      Rcpp::stop("Different grain (x,y) not allowed.");

    }

    // get row number; points in between rows go to the row below
    double row_id = std::floor((extent[3] - y) * grain_y);

    // last row must go up
    if (y == extent[2]) {

      row_id = dimensions[0] - 1;

    }

    // get col number; points in between cols go to the col right
    double col_id = std::floor((x - extent[0]) * grain_x);

    // last col must go left
    if (x == extent[1]) {

      col_id = dimensions[1] - 1;

    }

    // each increase in rows adds ncols cells
    // cell_id = row_id * dimensions[1] + col_id + 1;
    cell_id = row_id * dimensions[1] + col_id;

    // return rcpp index
    if (!rcpp) {

      cell_id += 1;

    }

    return cell_id;

  }
}

/*** R
# rcpp_cell_from_xy
x <- runif(n = 1, min = -25, max = 25)
y <- runif(n = 1, min = -25, max = 25)

rcpp_cell_from_xy(x = x, y = y, extent = c(-25, 25, -25, 25), dimensions = c(50, 50),
                  rcpp = FALSE)

terra::cellFromXY(object = terra::rast(nrows = 50, ncols = 50, xmin = -25, xmax = 25,
                                       ymin = -25, ymax = 25, crs = ""),
                  xy = cbind(x = x, y = y))

rcpp_cell_from_xy(x = 26, y = 0, extent = c(-25, 25, -25, 25), dimensions = c(50, 50),
                  rcpp = FALSE)
*/
