#include <Rcpp.h>
#include "rcpp_diffuse_values.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_diffuse_values
//'
//' @description
//' Rcpp simulate diffusion of values.
//'
//' @param seafloor Matrix with seafloor values.
//' @param cell_adj Matrix with cell adjacencies.
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
void rcpp_diffuse_values(Rcpp::NumericMatrix seafloor, Rcpp::IntegerMatrix cell_adj,
                         double nutrients_diffusion, double detritus_diffusion,
                         double detritus_fish_diffusion) {

  // get number of rows for cell adj and seafloor
  int n_cell = seafloor.nrow();

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

  // init counter for Queen's case
  int counter = 0;

  // add and remove diffused amounts
  // loop through all cells
  for (int j = 0; j < n_cell; j++) {

    // loop through all neighbors
    for (int k = 0; k < 8; k++) {

      // gut id of neighbor
      int neighbor = cell_adj(k + counter, 1);

      // add values of focal cell to neighbor cell
      seafloor(neighbor, 4) += nutrients[j];

      seafloor(neighbor, 5) += detritus[j];

      seafloor(neighbor, 6) += detritus_fish[j];

      // remove value from focal cell
      seafloor(j, 4) -= nutrients[j];

      seafloor(j, 5) -= detritus[j];

      seafloor(j, 6) -= detritus_fish[j];

    }

    // increase counter
    counter += 8;

  }
}

/*** R
rcpp_diffuse_values(seafloor = seafloor, cell_adj = cell_adj,
                    nutrients_diffusion = parameters$nutrients_diffusion,
                    detritus_diffusion = parameters$detritus_diffusion,
                    detritus_fish_diffusion = parameters$detritus_fish_diffusion)
*/
