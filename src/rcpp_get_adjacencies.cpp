#include <Rcpp.h>
#include "rcpp_get_adjacencies.h"

using namespace Rcpp;

//' rcpp_get_adjacencies
//'
//' @description
//' Rcpp get adjacencies
//'
//' @param dimensions Vector with number or rows and cols
//'
//' @details
//' Get matrix with cell IDs of all neighboring cells. Indices start with 0 according
//' to C++ indexing.
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
//' @export
// [[Rcpp::export]]
Rcpp::IntegerMatrix rcpp_get_adjacencies(Rcpp::IntegerVector dimensions) {

  int n_cell = dimensions[0] * dimensions[1];
  int global_counter = 0;
  int v_temp;

  Rcpp::IntegerMatrix adj(n_cell * 8, 2);

  for (int i = 0; i < n_cell; i++) {

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

      if (v_temp < 0) v_temp = n_cell + v_temp;

      // left <- (focal_id - 1) %% n_cell
      adj(global_counter, 1) = v_temp;

      global_counter++;

    }
  }

  return(adj);

}

/*** R

ras <- terra::rast(nrow = 5, ncol = 5)

get_neighbors(ras, direction = 8, cpp = TRUE)
rcpp_get_adjacencies(dim(ras)[1:2])

*/
