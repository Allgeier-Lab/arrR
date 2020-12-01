#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_calc_dist_reef
//'
//' @description Rcpp calc dist reef
//'
//' @param seafloor Matrix with coordinates of cells.
//' @param coords_reef 2-column Matrix with coordinates of AR.
//' @param extent Vector with dimension in x and y direction.
//' @param torus If TRUE the distance will be calculated using a torus.
//'
//' @details
//' Rcpp implementation to calculate distance to reef cells. Returns vector with
//' distance for each cell to reef
//'
//' @return vector
//'
//' @aliases rcpp_calc_dist_reef
//' @rdname rcpp_calc_dist_reef
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_calc_dist_reef(Rcpp::NumericMatrix seafloor,
                                        Rcpp::NumericMatrix coords_reef,
                                        Rcpp::NumericVector extent,
                                        bool torus = false) {


  // get dimension of seafloor and reef cells
  int n_cells = seafloor.nrow();

  int n_reef = coords_reef.nrow();

  // init distance double
  double dist_xy = 0.0;

  // init vector for distances filled with Inf to make sure first value is smaller
  Rcpp::NumericVector result(n_cells, R_PosInf);

  // loop through all cells
  for (int i = 0; i < n_cells; i++) {

    // loop through all reef cells
    for (int j = 0; j < n_reef; j++) {

      // calculate distance in x direction
      double dist_x = seafloor(i, 0) - coords_reef(j, 0);

      // calculate distance in y direction
      double dist_y = seafloor(i, 1) - coords_reef(j, 1);

      // don't use torus for distance
      if (torus == false) {

        dist_xy = std::sqrt(std::pow(dist_x, 2.0) + std::pow(dist_y, 2.0));

      // distance is calculated on torus
      } else {

        dist_x = std::pow(std::min(std::abs(dist_x), extent(0) - std::abs(dist_x)), 2.0);

        dist_y = std::pow(std::min(std::abs(dist_y), extent(1) - std::abs(dist_y)), 2.0);

        dist_xy = std::sqrt(dist_x + dist_y);

      }

      // check if current distance is smaller than previous smallest distance
      if (dist_xy < result(i)) {

        result(i) = dist_xy;

      }
    }
  }

  return(result);

}

/*** R
# create seafloor
input_seafloor <- setup_seafloor(extent = c(50, 50), grain = c(1, 1),
                                 reefs = reef_matrix,
                                 starting_values = starting_values,
                                 parameters = parameters)

plot(input_seafloor$reef_dist)
*/
