#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_calc_dist_reef
//'
//' @description Internal rcpp function
//'
//' @param fish_population 2-column matrix with coordinates of individual fish.
//' @param coords_reef 2-column matrix with coordinates of AR.
//'
//' @details
//' Internal function calculate distance to reef cells.
//'
//' @return matrix
//'
//' @aliases rcpp_calc_dist_reef
//' @rdname rcpp_calc_dist_reef
//'
//' @keywords internal
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_calc_dist_reef(Rcpp::NumericMatrix seafloor,
                                        Rcpp::NumericMatrix coords_reef) {

  int n_cells = seafloor.nrow();
  int n_reef = coords_reef.nrow();

  Rcpp::NumericVector result(n_cells, R_PosInf);

  for (int i = 0; i < n_cells; i++) {

    for (int j = 0; j < n_reef; j++) {

      double dist_x = seafloor(i, 0) - coords_reef(j, 0);

      double dist_y = seafloor(i, 1) - coords_reef(j, 1);

      double dist_xy = std::sqrt(dist_x * dist_x + dist_y * dist_y);

      if (dist_xy < result(i)) {

        result(i) = dist_xy;

      }
    }
  }

  return(result);

}

/*** R
# create seafloor
input_seafloor <- setup_seafloor(extent = c(50, 50), grain = 1, reefs = reef_matrix,
                                 starting_values = starting_values, parameters = parameters)

# create population
input_fish_population <- setup_fish_population(seafloor = input_seafloor,
                                               starting_values = starting_values,
                                               parameters = parameters)

# get coordinates of reef cells
coords_reef <- raster::xyFromCell(object = seafloor$reef,
                                  cell = cells_reef)

foo_rcpp <- function(){rcpp_calc_dist_reef(as.matrix(fish_population[, c(3,4)]), coords_reef)}
foo_r <- function(){int_calc_dist_reef(fish_population[, c(3, 4)], coords_reef)}

bench::mark(foo_rcpp(), foo_r(),
            check = FALSE, relative = TRUE, iterations = 1000)
*/
