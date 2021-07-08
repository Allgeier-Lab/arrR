#include "rcpp_diffuse_values.h"
#include "rcpp_get_neighbors.h"

//' rcpp_diffuse_values
//'
//' @description
//' Rcpp simulate diffusion of values.
//'
//' @param seafloor Matrix with seafloor values.
//' @param n_cell,n_col Integer with number of cells and columns.
//' @param nutrients_diffusion,detritus_diffusion,detritus_fish_diffusion Numeric with parameters.
//'
//' @details
//' Simulates the diffusion of a certain share of i) nutrients_pool, ii) detritus_pool,
//' and iii) detritus_fish_diffusion of each cell with its neighboring cells. Scheduling
//' is simulated pseudo-simultaneous.
//'
//' @return void
//'
//' @aliases rcpp_diffuse_values
//' @rdname rcpp_diffuse_values
//'
//' @export
// [[Rcpp::export]]
void rcpp_diffuse_values(Rcpp::NumericMatrix seafloor, int n_cell, int n_col,
                         double nutrients_diffusion, double detritus_diffusion,
                         double detritus_fish_diffusion) {

  // create vectors to store seafloor values
  Rcpp::NumericVector nutrients (n_cell);

  Rcpp::NumericVector detritus (n_cell);

  Rcpp::NumericVector detritus_fish (n_cell);

  // get all seafloor values
  for (int i = 0; i < n_cell; i++) {

    nutrients(i) = (seafloor(i, 4) * nutrients_diffusion) / 8.0;

    detritus(i) = (seafloor(i, 5) * detritus_diffusion) / 8.0;

    detritus_fish(i) = (seafloor(i, 6) * detritus_fish_diffusion) / 8.0;

  }

  // get all seafloor values
  for (int i = 0; i < n_cell; i++) {

    Rcpp::IntegerVector neighbooring = rcpp_get_neighbors(i, n_cell, n_col, 8);

    for (int j = 0; j < neighbooring.length(); j++) {

      int neighbor_temp = neighbooring(j);

      // add values of focal cell to neighbor cell
      seafloor(neighbor_temp, 4) += nutrients(i);

      seafloor(neighbor_temp, 5) += detritus(i);

      seafloor(neighbor_temp, 6) += detritus_fish(i);

      // remove value from focal cell
      seafloor(i, 4) -= nutrients(i);

      seafloor(i, 5) -= detritus(i);

      seafloor(i, 6) -= detritus_fish(i);
    }
  }
}

/*** R
rcpp_diffuse_values(seafloor = seafloor, n_cell = n_cell, n_col = dimensions(2),
                    nutrients_diffusion = parameters$nutrients_diffusion,
                    detritus_diffusion = parameters$detritus_diffusion,
                    detritus_fish_diffusion = parameters$detritus_fish_diffusion)
*/
