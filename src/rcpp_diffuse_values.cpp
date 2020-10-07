#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_diffuse_values
//'
//' @description Rcpp sample function
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
  int n_row_adj = cell_adj.nrow();
  int n_row_sea = seafloor_values.nrow();

  // create vectors to store seafloor values
  Rcpp::NumericVector nutrients (n_row_sea);
  Rcpp::NumericVector detritus (n_row_sea);
  Rcpp::NumericVector detritus_dead (n_row_sea);

  // get all seafloor values
  for(int i = 0; i < n_row_sea; i++) {

    nutrients(i) = (seafloor_values(i, 4) * nutrients_diffusion) / 8;
    detritus(i) = (seafloor_values(i, 5) * detritus_diffusion) / 8;
    detritus_dead(i) = (seafloor_values(i, 6) * detritus_dead_diffusion) / 8;

  }

  for (int j = 0; j < n_row_adj; j++) {

    //  get current focal and neighbor cell; C++ starts at 0
    int focal = cell_adj(j, 0) - 1;
    int neighbor = cell_adj(j, 1) - 1;

    // add values of focal cell to neighbor cell
    seafloor_values(neighbor, 4) += nutrients(focal);
    seafloor_values(neighbor, 5) += detritus(focal);
    seafloor_values(neighbor, 6) += detritus_dead(focal);

    // remove value from focal cell
    seafloor_values(focal, 4) -= nutrients(focal);
    seafloor_values(focal, 5) -= detritus(focal);
    seafloor_values(focal, 6) -= detritus_dead(focal);

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
