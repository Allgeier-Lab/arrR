#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_diffuse_values
//'
//' @description Rcpp sample function
//'
//' @param x Vector of elements to sample from.
//' @param n Size of the sample.
//' @param replace Sample with replacement.
//'
//' @details
//' \code{Rcpp} implementation of the \code{sample} function.
//'
//' @seealso
//' \code{\link{sample}}
//'
//' @return vector
//'
//' @name rcpp_diffuse_values
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_diffuse_values(Rcpp::NumericMatrix seafloor_values,
                                        Rcpp::NumericMatrix cell_adj,
                                        double wc_diffusion,
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

    nutrients(i) = (seafloor_values(i, 6) * wc_diffusion) / 8;
    detritus(i) = (seafloor_values(i, 4) * detritus_diffusion) / 8;
    detritus_dead(i) = (seafloor_values(i, 5) * detritus_dead_diffusion) / 8;

  }

  // // get random sample of cell adj rows
  // Rcpp::IntegerVector row_id = Rcpp::seq_len(n_row_adj);
  // Rcpp::IntegerVector random_id = Rcpp::sample(row_id, n_row_adj);

  for (int j = 0; j < n_row_adj; j++) {

    // int k = random_id(j);

    //  get current focal and neighbor cell; C++ starts at 0
    int focal = cell_adj(j, 0) - 1;
    int neighbor = cell_adj(j, 1) - 1;

    // add values of focal cell to neighbor cell
    seafloor_values(neighbor, 6) += nutrients(focal);
    seafloor_values(neighbor, 4) += detritus(focal);
    seafloor_values(neighbor, 5) += detritus_dead(focal);

    // remove value from focal cell
    seafloor_values(focal, 6) -= nutrients(focal);
    seafloor_values(focal, 4) -= detritus(focal);
    seafloor_values(focal, 5) -= detritus_dead(focal);

  }

  return seafloor_values;

}

/*** R
rcpp_diffuse_values(seafloor_values = as.matrix(seafloor_values),
                    cell_adj = cell_adj,
                    wc_diffusion = parameters$wc_diffusion,
                    detritus_diffusion = parameters$detritus_diffusion,
                    detritus_dead_diffusion = parameters$detritus_dead_diffusion)
*/
