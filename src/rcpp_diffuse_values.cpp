#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_diffuse_values
//'
//' @description Rcpp diffuse values
//'
//' @param seafloor Matrix with seafloor values.
//' @param cell_adj Matrix with cell adjacencies.
//' @param nutrients_diffusion,detritus_diffusion,detritus_dead_diffusion Numeric with parameters.
//'
//' @details
//' Rcpp implementation to diffuse seafloor values between (queen case) neighbouring cells.
//'
//' @return void
//'
//' @aliases rcpp_diffuse_values
//' @rdname rcpp_diffuse_values
//'
//' @export
// [[Rcpp::export]]
void rcpp_diffuse_values(Rcpp::NumericMatrix seafloor,
                         Rcpp::NumericMatrix cell_adj,
                         double nutrients_diffusion,
                         double detritus_diffusion,
                         double detritus_dead_diffusion) {

  // get number of rows for cell adj and seafloor
  int n_row_cell_adj = cell_adj.nrow();

  int n_row_seafloor = seafloor.nrow();

  // create vectors to store seafloor values
  Rcpp::NumericVector nutrients (n_row_seafloor);

  Rcpp::NumericVector detritus (n_row_seafloor);

  Rcpp::NumericVector detritus_dead (n_row_seafloor);

  // get all seafloor values
  for (int i = 0; i < n_row_seafloor; i++) {

    nutrients(i) = (seafloor(i, 4) * nutrients_diffusion) / 8.0;

    detritus(i) = (seafloor(i, 5) * detritus_diffusion) / 8.0;

    detritus_dead(i) = (seafloor(i, 6) * detritus_dead_diffusion) / 8.0;

  }

  // add and remove diffused amounts
  for (int j = 0; j < n_row_cell_adj; j++) {

    //  get current focal and neighbor cell; C++ starts at 0
    int focal = cell_adj(j, 0) - 1;

    int neighbor = cell_adj(j, 1) - 1;

    // add values of focal cell to neighbor cell
    seafloor(neighbor, 4) += nutrients(focal);

    seafloor(neighbor, 5) += detritus(focal);

    seafloor(neighbor, 6) += detritus_dead(focal);

    // remove value from focal cell
    seafloor(focal, 4) -= nutrients(focal);

    seafloor(focal, 5) -= detritus(focal);

    seafloor(focal, 6) -= detritus_dead(focal);

  }
}

/*** R
rcpp_diffuse_values(seafloor = seafloor,
                    cell_adj = cell_adj,
                    nutrients_diffusion = parameters$nutrients_diffusion,
                    detritus_diffusion = parameters$detritus_diffusion,
                    detritus_dead_diffusion = parameters$detritus_dead_diffusion)
*/
