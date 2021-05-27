#include "rcpp_diffuse_values.h"

//' rcpp_diffuse_values
//'
//' @description Rcpp diffuse values
//'
//' @param seafloor Matrix with seafloor values.
//' @param cell_adj Matrix with cell adjacencies.
//' @param nutrients_diffusion,detritus_diffusion,detritus_fish_diffusion Numeric with parameters.
//'
//' @details
//' A certain share of each cell value, specified by the diffusion parameters, is
//' diffused to its 8 neighboring cells.
//'
//' @return void
//'
//' @aliases rcpp_diffuse_values
//' @rdname rcpp_diffuse_values
//'
//' @export
// [[Rcpp::export]]
void rcpp_diffuse_values(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix cell_adj,
                         double nutrients_diffusion, double detritus_diffusion,
                         double detritus_fish_diffusion) {

  // get number of rows for cell adj and seafloor
  int n_row_cell_adj = cell_adj.nrow();

  int n_row_seafloor = seafloor.nrow();

  // create vectors to store seafloor values
  Rcpp::NumericVector nutrients (n_row_seafloor);

  Rcpp::NumericVector detritus (n_row_seafloor);

  Rcpp::NumericVector detritus_fish (n_row_seafloor);

  // get all seafloor values
  for (int i = 0; i < n_row_seafloor; i++) {

    nutrients(i) = (seafloor(i, 4) * nutrients_diffusion) / 8.0;

    detritus(i) = (seafloor(i, 5) * detritus_diffusion) / 8.0;

    detritus_fish(i) = (seafloor(i, 6) * detritus_fish_diffusion) / 8.0;

  }

  // add and remove diffused amounts
  for (int j = 0; j < n_row_cell_adj; j++) {

    //  get current focal and neighbor cell; C++ starts at 0
    int focal = cell_adj(j, 0) - 1;

    int neighbor = cell_adj(j, 1) - 1;

    // add values of focal cell to neighbor cell
    seafloor(neighbor, 4) += nutrients(focal);

    seafloor(neighbor, 5) += detritus(focal);

    seafloor(neighbor, 6) += detritus_fish(focal);

    // remove value from focal cell
    seafloor(focal, 4) -= nutrients(focal);

    seafloor(focal, 5) -= detritus(focal);

    seafloor(focal, 6) -= detritus_fish(focal);

  }
}

/*** R
rcpp_diffuse_values(seafloor = seafloor,
                    cell_adj = cell_adj,
                    nutrients_diffusion = parameters$nutrients_diffusion,
                    detritus_diffusion = parameters$detritus_diffusion,
                    detritus_fish_diffusion = parameters$detritus_fish_diffusion)
*/
