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

  // init counter for Queen's case
  int counter = 0;

  // create vectors to store seafloor values
  Rcpp::NumericVector nutrients (n_cell);

  Rcpp::NumericVector detritus (n_cell);

  Rcpp::NumericVector detritus_fish (n_cell);

  // for sean, calculating diffusion in/out of reef
  int calc_range = 5;
  std::vector<bool> edge_reef (n_cell);
  std::vector<bool> edge_open (n_cell);

  // get all seafloor values
  for (int i = 0; i < n_cell; i++) {

    nutrients(i) = (seafloor(i, 4) * nutrients_diffusion) / 8.0;

    detritus(i) = (seafloor(i, 5) * detritus_diffusion) / 8.0;

    detritus_fish(i) = (seafloor(i, 6) * detritus_fish_diffusion) / 8.0;

    // extent of reef (inclusive)
    if((seafloor(i, 0) == (seafloor(1275,0) + calc_range) && seafloor(i,1) <=
       (seafloor(1275,1) + calc_range) && seafloor(i, 1) >= (seafloor(1275,1) - calc_range)) ||
       (seafloor(i, 0) == seafloor(1275,0) - calc_range && seafloor(i,1) <=
       (seafloor(1275,1) + calc_range) && seafloor(i, 1) >= (seafloor(1275,1) - calc_range)) ||
       (seafloor(i, 1) == seafloor(1275,1) + calc_range && seafloor(i,0) <=
       (seafloor(1275,0) + calc_range) && seafloor(i, 0) >= (seafloor(1275,0) - calc_range)) ||
       (seafloor(i, 1) == seafloor(1275,1) - calc_range && seafloor(i,0) <=
       (seafloor(1275,0) + calc_range) && seafloor(i, 0) >= (seafloor(1275,0) - calc_range))) {
      edge_reef[i] = TRUE;
    }
    else {
      edge_reef[i] = FALSE;
    }

    // extent of reef (exclusive), open starts here
    if((seafloor(i, 0) == seafloor(1275,0) + (calc_range + 1) && seafloor(i,1) <=
       (seafloor(1275,1) + (calc_range + 1)) && seafloor(i, 1) >= (seafloor(1275,1) - (calc_range + 1))) ||
       (seafloor(i, 0) == seafloor(1275,0) - (calc_range + 1) && seafloor(i,1) <=
       (seafloor(1275,1) + (calc_range + 1)) && seafloor(i, 1) >= (seafloor(1275,1) - (calc_range + 1))) ||
       (seafloor(i, 1) == seafloor(1275,1) + (calc_range + 1) && seafloor(i,0) <=
       (seafloor(1275,0) + (calc_range + 1)) && seafloor(i, 0) >= (seafloor(1275,0) - (calc_range + 1))) ||
       (seafloor(i, 1) == seafloor(1275,1) - (calc_range + 1) && seafloor(i,0) <=
       (seafloor(1275,0) + (calc_range + 1)) && seafloor(i, 0) >= (seafloor(1275,0) - (calc_range + 1)))) {
      edge_open[i] = TRUE;
    }
    else{
      edge_open[i] = FALSE;
    }
  }

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
      if (edge_reef[neighbor] == TRUE && edge_open[j] == TRUE) {
        seafloor(j, 17) += nutrients[j];
        seafloor(neighbor, 16) += nutrients[j];
      }
      if (edge_open[neighbor] == TRUE&& edge_reef[j] == TRUE) {
        seafloor(j, 17) += nutrients[j];
        seafloor(neighbor, 16) += nutrients[j];
      }
      // remove value from focal cell
      double temp = seafloor(j, 4); seafloor(j, 4) -= nutrients[j];

      seafloor(j, 5) -= detritus[j];

      seafloor(j, 6) -= detritus_fish[j];

    }

    // increase counter
    counter += 8;
  }
}
