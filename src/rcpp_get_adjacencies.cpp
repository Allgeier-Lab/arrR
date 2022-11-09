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

  // calculate total number of cells
  int n_cell = dimensions[0] * dimensions[1];

  // init counter for cell and neighboor ids
  int global_counter = 0;

  // init matrix to store adjacenciesacencies
  Rcpp::IntegerMatrix adjacencies (n_cell * 8, 2);

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
      int id_temp = -9999;

      // set current focal cell
      adjacencies(global_counter, 0) = i;

      // top
      if (j == 0) {

        // torus translation first row
        if (row_id == 0) {if (torus) {id_temp = row_last * dimensions[1] + col_id;}}

        // cell one row above
        else {id_temp = (row_id - 1) * dimensions[1] + col_id;}

      // top-right
      } else if (j == 1) {

        // torus translation first row
        if (row_id == 0 && col_id < col_last) {if (torus) {id_temp = row_last * dimensions[1] + (col_id + 1);}}

        // torus translation corner cell
        else if (row_id == 0 && col_id == col_last) {if (torus) {id_temp = row_last * dimensions[1] + 0;}}

        // torus translation last column
        else if (row_id > 0 && col_id == col_last) {if (torus) {id_temp = (row_id - 1) * dimensions[1] + 0;}}

        // cell one row above, one column right
        else {id_temp = (row_id - 1) * dimensions[1] + (col_id + 1);}

      // right
      } else if (j == 2) {

        // torus translation last column
        if (col_id == col_last) {if (torus) {id_temp = row_id * dimensions[1] + 0;}}

        // cell one column right
        else {id_temp = row_id * dimensions[1] + (col_id + 1);}

      // bottom-right
      } else if (j == 3) {

        // torus translation last row
        if (row_id == row_last && col_id < col_last) {if (torus) {id_temp = 0 * dimensions[1] + (col_id + 1);}}

        // torus translation corner cell
        else if (row_id == row_last && col_id == col_last) {if (torus) {id_temp = 0 * dimensions[1] + 0;}}

        // torus translation las column
        else if (row_id < row_last && col_id == col_last) {if (torus) {id_temp = (row_id + 1) * dimensions[1] + 0;}}

        // cell one row abouve ane one column right
        else {id_temp = (row_id + 1) * dimensions[1] + (col_id + 1);}

      // bottom
      } else if (j == 4) {

        // torus translation last row
        if (row_id == row_last) {if (torus) {id_temp =  0 * dimensions[1] + col_id;}}

        // cell one row below
        else {id_temp = (row_id + 1) * dimensions[1] + col_id;}

      // bottom-left
      } else if (j == 5) {

        // torus translation last row
        if (row_id == row_last && col_id > 0) {if (torus) {id_temp = 0 * dimensions[1] + (col_id - 1);}}

        // torus translation corner cell
        else if (row_id == row_last && col_id == 0) {if (torus) {id_temp = 0 * dimensions[1] + col_last;}}

        // torus translation firt column
        else if (row_id < row_last && col_id == 0) {if (torus) {id_temp = (row_id + 1) * dimensions[1] + col_last;}}

        // cell one row left and one column below
        else {id_temp = (row_id + 1) * dimensions[1] + (col_id - 1);}

      // left
      } else if (j == 6) {

        // torus translation first row
        if (col_id == 0) {if (torus) {id_temp = row_id * dimensions[1] + col_last;}}

        // cell one column to left
        else {id_temp = row_id * dimensions[1] + (col_id - 1);}

      // top-left
      } else if (j == 7) {

        // torus translation first row
        if (row_id == 0 && col_id > 0) {if (torus) {id_temp = row_last * dimensions[1] + (col_id - 1);}}

        // torus translation corner cell
        else if (row_id == 0 && col_id == 0) {if (torus) {id_temp = row_last * dimensions[1] + col_last;}}

        // torus translation first column
        else if (row_id > 0 && col_id == 0) {if (torus) {id_temp = (row_id - 1) * dimensions[1] + col_last;}}

        // cell one row left and one column above
        else {id_temp = (row_id - 1) * dimensions[1] + (col_id - 1);}

      }

      // set current neighbor cell id
      adjacencies(global_counter, 1) = id_temp;

      // increase counter for adjacencies matrix
      global_counter++;

    }
  }

  // remove NA rows if no torus was used
  if (!torus) {

    // init vector to store NA values
    Rcpp::IntegerVector itr;

    // loop all adjacenciesacencies
    for (int i = 0; i < adjacencies.nrow(); i++) {

      // store current iterations if not NA
      if (adjacencies(i, 1) != -9999) {itr.push_back (i);}

    }

    // init new matrix for cleaned adjacenciesacencies
    Rcpp::IntegerMatrix adjacencies_clean (itr.length(), 2);

    // loop through all NAs
    for (int j = 0; j < itr.length(); j++) {

      // store non NA values
      adjacencies_clean(j, _) = adjacencies(itr[j], _);

    }

    return (adjacencies_clean);

  } else {

    return(adjacencies);

  }
}

/*** R
r <- 3
c <- 3
ras <- terra::rast(nrow = r, ncol = c, crs = NA, extent = c(0, r, 0, c))

rcpp_get_adjacencies(dimensions = dim(ras)[1:2], torus = FALSE)
rcpp_get_adjacencies(dimensions = dim(ras)[1:2], torus = TRUE)
*/

