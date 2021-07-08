#include "rcpp_get_neighbors.h"

//' rcpp_get_neighbors
//'
//' @description
//' Rcpp get id of neighboring cells
//'
//' @param id Integer with id of focal cell.
//' @param n_col,n_cell Integer with number of columns and cells.
//' @param directions Integer specifying neighborhood rule
//'
//' @details
//' Returns a vector with cell ID of neighboring cells. The direction argument can be
//' set to \code{directions = 4} (Rook's case) or \code{directions = 8} (Queen's case).
//' The returning vector gives the ID in the following order: left, top, right, bottom or
//' left, top_left, top, top_right, right, bottom_right, bottom, bottom_left.
//'
//' @return vector
//'
//' @aliases rcpp_get_neighbors
//' @rdname rcpp_get_neighbors
//'
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_get_neighbors(int id, int n_cell,  int n_col, int directions) {

  // init vector to store results
  Rcpp::IntegerVector neighbors (directions, 0);

  int left = (id - 1) % n_cell; if (left < 0) left += n_cell;

  int top = (id - n_col) % n_cell; if (top < 0) top += n_cell;

  int right = (id + 1) % n_cell; if (right < 0) right += n_cell;

  int bottom = (id + n_col) % n_cell; if (bottom < 0) bottom += n_cell;

  // Rook's case
  if (directions == 4) {

    neighbors(0) = left; neighbors(1) = top;

    neighbors(2) = right; neighbors(3) = bottom;

    // Queen's case
  } else if (directions == 8) {

    int top_left = (id - (n_col + 1)) % n_cell; if (top_left < 0) top_left += n_cell;

    int top_right = (id - (n_col - 1)) % n_cell; if (top_right < 0) top_right += n_cell;

    int bottom_right = (id + (n_col + 1)) % n_cell; if (bottom_right < 0) bottom_right += n_cell;

    int bottom_left = (id + (n_col - 1)) % n_cell; if (bottom_left < 0) bottom_left += n_cell;

    neighbors(0) = left; neighbors(1) = top_left; neighbors(2) = top; neighbors(3) = top_right;

    neighbors(4) = right; neighbors(5) = bottom_right; neighbors(6) = bottom; neighbors(7) = bottom_left;

  // check directions argument
  } else {

    Rcpp::stop("Please select either direction = 4 or direction = 8.");

  }

  return(neighbors);
}

/*** R
rcpp_get_neighbors(id = 0, n_cell = 2500, n_col = 50, directions = 8)
*/
