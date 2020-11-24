#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_diffuse_values
//'
//' @description Rcpp function to diffuse seafloor values
//'
//' @param seafloor_values Matrix with seafloor values.
//' @param cell_adj Matrix with cell adjacencies.
//' @param nutrients_diffusion,detritus_diffusion,detritus_dead_diffusion Numeric with parameters.
//'
//' @details
//' \code{Rcpp} implementation of to diffuse nutrients.
//'
//' @return Matrix
//'
//' @name rcpp_diffuse_values
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_diffuse_values(Rcpp::NumericMatrix seafloor_values,
                                        Rcpp::NumericMatrix cell_adj,
                                        double nutrients_diffusion,
                                        double detritus_diffusion,
                                        double detritus_dead_diffusion) {

  // get number of rows for cell adj and seafloor
  int n_row_cell_adj = cell_adj.nrow();
  int n_row_seafloor = seafloor_values.nrow();

  // create vectors to store seafloor values
  Rcpp::NumericVector nutrients (n_row_seafloor);
  Rcpp::NumericVector detritus (n_row_seafloor);
  Rcpp::NumericVector detritus_dead (n_row_seafloor);

  // get all seafloor values
  for(int i = 0; i < n_row_seafloor; i++) {

    nutrients(i) = (seafloor_values(i, 0) * nutrients_diffusion) / 8;
    detritus(i) = (seafloor_values(i, 1) * detritus_diffusion) / 8;
    detritus_dead(i) = (seafloor_values(i, 2) * detritus_dead_diffusion) / 8;

  }

  // add and remove diffused amounts
  for (int j = 0; j < n_row_cell_adj; j++) {

    //  get current focal and neighbor cell; C++ starts at 0
    int focal = cell_adj(j, 0) - 1;
    int neighbor = cell_adj(j, 1) - 1;

    // add values of focal cell to neighbor cell
    seafloor_values(neighbor, 0) += nutrients(focal);
    seafloor_values(neighbor, 1) += detritus(focal);
    seafloor_values(neighbor, 2) += detritus_dead(focal);

    // remove value from focal cell
    seafloor_values(focal, 0) -= nutrients(focal);
    seafloor_values(focal, 1) -= detritus(focal);
    seafloor_values(focal, 2) -= detritus_dead(focal);

  }

  return seafloor_values;

}

/*** R
rcpp_diffuse_values(seafloor_values = as.matrix(seafloor_values),
                    cell_adj = cell_adj,
                    nutrients_diffusion = parameters$nutrients_diffusion,
                    detritus_diffusion = parameters$detritus_diffusion,
                    detritus_dead_diffusion = parameters$detritus_dead_diffusion)

*/


