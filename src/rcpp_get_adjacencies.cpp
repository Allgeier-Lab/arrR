#include <Rcpp.h>

#include "rcpp_get_adjacencies.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_get_adjacencies
//'
//' @description
//' Rcpp get adjacencies
//'
//' @param dimensions Vector with number or rows and cols
//' @param torus Logical if true, torus translation is used.
//'
//' @details
//' Get matrix with cell IDs of all neighboring cells. Indices start with 0
//' according to C++ indexing.
//'
//' @references
//' Code adapted from Robert J. Hijmans (2020). raster: Geographic Data Analysis
//' and Modeling. R package version 3.4-5. <https://CRAN.R-project.org/package=raster>
//'
//' @return matrix
//'
//' @aliases rcpp_get_adjacencies
//' @rdname rcpp_get_adjacencies
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::IntegerMatrix rcpp_get_adjacencies(Rcpp::IntegerVector dimensions, bool torus) {

  int n_cell = dimensions[0] * dimensions[1];

  int global_counter = 0;

  Rcpp::IntegerMatrix adj(n_cell * 8, 2);

  if (torus) {

    for (int i = 0; i < n_cell; i++) {

      int v_temp = NA_REAL;

      for (int j = 0; j < 8; j++) {

        adj(global_counter, 0) = i;

        // left
        if (j == 0) {

          v_temp = (i - 1) % n_cell;

        // top-left
        } else if (j == 1) {

          v_temp = (i - (dimensions[1] + 1)) % n_cell;

        // top
        } else if (j == 2) {

          v_temp = (i - dimensions[1]) % n_cell;

        // top-right
        } else if (j == 3) {

          v_temp = (i - (dimensions[1] - 1)) % n_cell;

        // right
        } else if (j == 4) {

          v_temp = (i + 1) % n_cell;

        // bottom-right
        } else if (j == 5) {

          v_temp = (i + (dimensions[1] + 1)) % n_cell;

        // bottom
        } else if (j == 6) {

          v_temp = (i + dimensions[1]) % n_cell;

        // bottom-left
        } else {

          v_temp = (i + (dimensions[1] - 1)) % n_cell;

        }

        if ((v_temp < 0)) v_temp = n_cell + v_temp;

        // left <- (focal_id - 1) %% n_cell
        adj(global_counter, 1) = v_temp;

        global_counter++;

      }
    }

  } else {

    // get last row and col
    int row_last = dimensions[0] - 1;
    int col_last = dimensions[1] - 1;

    // loop through all cells
    for(int i = 0; i < n_cell; i++) {

      // get current row and col id
      int row_id = std::trunc(i / dimensions[1]);
      int col_id = (i - (row_id * dimensions[1]));

      // loop through all neighbors
      for (int j = 0; j < 8; j++) {

        // reset current neighbor id
        int v_temp = NA_REAL;

        // set current focal cell
        adj(global_counter, 0) = i;

        // top
        if ((j == 0) & (row_id > 0)) {

          v_temp = (row_id - 1) * dimensions[1] + col_id;

        // top-right
        } else if ((j == 1) & (row_id > 0) & (col_id < col_last)) {

          v_temp = (row_id - 1) * dimensions[1] + (col_id + 1);

        // right
        } else if ((j == 2) & (col_id < col_last)) {

          v_temp = row_id * dimensions[1] + (col_id + 1);

        // bottom-right
        } else if ((j == 3) & (row_id < row_last) & (col_id < col_last)) {

          v_temp = (row_id + 1) * dimensions[1] + (col_id + 1);

        // bottom
        } else if ((j == 4) & (row_id < row_last)) {

          v_temp = (row_id + 1) * dimensions[1] + col_id;

        // bottom-left
        } else if ((j == 5) & (row_id < row_last) & (col_id > 0)) {

          v_temp = (row_id + 1) * dimensions[1] + (col_id - 1);

        // left
        } else if ((j == 6) & (col_id > 0)) {

          v_temp = row_id * dimensions[1] + (col_id - 1);

        // top-left
        } else if ((j == 7) & (row_id > 0) & (col_id > 0)) {

          v_temp = (row_id - 1) * dimensions[1] + (col_id - 1);

        }

        // set current neighbor cell id
        adj(global_counter, 1) = v_temp;

        global_counter++;

      }
    }
  }

  return(adj);

}

/*** R
r <- 4
c <- 4
ras <- terra::rast(nrow = r, ncol = c, crs = NA, extent = c(0, r, 0, c))

rcpp_get_adjacencies(dim(ras)[1:2], TRUE)

rcpp_get_adjacencies(dim(ras)[1:2], FALSE) |> as.data.frame() |> dplyr::filter(!is.na(V2))

terra::adjacent(x = ras, cells = 1:16, pairs = TRUE, directions = 8) |> as.data.frame()

*/
