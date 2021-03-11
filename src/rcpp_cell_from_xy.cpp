#include "rcpp_cell_from_xy.h"

//' rcpp_cell_from_xy
//'
//' @description Rcpp get cell id from xy
//'
//' @param coords Vector with coordinates.
//' @param dimensions Vector with number or rows and cols
//' @param extent Vector with extent (xmin, xmax, ymin, ymax).
//'
//' @details
//' Rcpp implementation to get cell id from xy coordinate. Allows only one coordinate
//' pair at a time.
//'
//' @references
//' Code adapted from Robert J. Hijmans (2020). raster: Geographic Data Analysis
//' and Modeling. R package version 3.4-5. https://CRAN.R-project.org/package=raster
//'
//' @return int
//'
//' @aliases rcpp_cell_from_xy
//' @rdname rcpp_cell_from_xy
//'
//' @keywords export
// [[Rcpp::export]]
int rcpp_cell_from_xy(Rcpp::NumericVector coords,
                      Rcpp::NumericVector dimensions,
                      Rcpp::NumericVector extent) {

  // coords outside extent; return NA
  if (coords(0) < extent(0) || coords(0) > extent(1) ||
      coords(1) < extent(2) || coords(1) > extent(3)) {

    int cell_id = NA_REAL;

    return cell_id;

  // coords within extent
  } else {

    // calculates resolution
    double grain_x = dimensions(1) / (extent(1) - extent(0));

    double grain_y = dimensions(0) / (extent(3) - extent(2));

    // get row number; points in between rows go to the row below
    double row_id = floor((extent(3) - coords(1)) * grain_y);

    // last row must go up
    if (coords(1) == extent(2)) {

      row_id = dimensions(0) - 1 ;

    }

    // get col number; points in between cols go to the col right
    double col_id = floor((coords(0) - extent(0)) * grain_x);

    // last col must go left
    if (coords(0) == extent(1)) {

      col_id = dimensions(1) - 1;

    }

    // each increase in rows adds ncols cells
    int cell_id = row_id * dimensions(1) + col_id + 1;

    return cell_id;

  }
}

/*** R

# rcpp_cell_from_xy
x <- runif(n = 1, min = -50, max = 50)
y <- runif(n = 1, min = -50, max = 50)

rcpp_cell_from_xy(dimensions = c(100, 100),
                  extent = c(-50, 50, -50, 50),
                  cbind(x = x, y = y))

raster::cellFromXY(object = raster::raster(nrows = 100, ncols = 100,
                                           xmn = -50, xmx = 50,
                                           ymn = -50, ymx = 50),
                   xy = cbind(x = x, y = y))

*/