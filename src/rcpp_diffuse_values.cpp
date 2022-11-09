#include <Rcpp.h>

#include "rcpp_diffuse_values.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_diffuse_values
//'
//' @description
//' Rcpp simulate diffusion.
//'
//' @param seafloor Matrix with seafloor values.
//' @param cell_adj Matrix with cell adjacencies.
//' @param nutrients_diffusion,detritus_diffusion,detritus_fish_diffusion Numeric with parameters.
//'
//' @details
//' Simulates the diffusion of the i) nutrients_pool, ii) detritus_pool,
//' and iii) detritus_fish_diffusion of each cell with its neighboring cells.
//' Scheduling is simulated pseudo-simultaneous.
//'
//' @return void
//'
//' @aliases rcpp_diffuse_values
//' @rdname rcpp_diffuse_values
//'
//' @keywords internal
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

  // calculate diffusion amount and remove amount from focal cells
  for (int i = 0; i < n_cell; i++) {

    nutrients[i] = seafloor(i, 4) * nutrients_diffusion;
    seafloor(i, 4) -= nutrients[i];

    detritus[i] = seafloor(i, 5) * detritus_diffusion;
    seafloor(i, 5) -= detritus[i];

    detritus_fish[i] = seafloor(i, 6) * detritus_fish_diffusion;
    seafloor(i, 6) -= detritus_fish[i];

  }

  // diffuse values
  for (int j = 0; j < cell_adj.nrow(); j++) {

    // get focal and neighboring cell
    int focal = cell_adj(j, 0);
    int neighbor = cell_adj(j, 1);

    // add values of focal cell to neighbor cell
    seafloor(neighbor, 4) += nutrients[focal] / 8.0;

    seafloor(neighbor, 5) += detritus[focal] / 8.0;

    seafloor(neighbor, 6) += detritus_fish[focal] / 8.0;

  }
}
