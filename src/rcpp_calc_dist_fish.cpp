#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_calc_dist_fish
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
//' @aliases rcpp_calc_dist_fish
//' @rdname rcpp_calc_dist_fish
//'
//' @keywords internal
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_calc_dist_fish(Rcpp::NumericMatrix fish_population,
                                        Rcpp::NumericMatrix coords_reef) {

  // Init all needed objects
  int n_pop = fish_population.nrow();
  int n_reef = coords_reef.nrow();

  Rcpp::NumericMatrix result(n_pop, 2);
  result(_, 0) = Rcpp::NumericVector (n_pop, R_PosInf);

  for (int i = 0; i < n_pop; i++) {

    for (int j = 0; j < n_reef; j++) {

      double dist_x = fish_population(i, 0) - coords_reef(j, 0);

      double dist_y = fish_population(i, 1) - coords_reef(j, 1);

      double dist_xy = std::sqrt(dist_x * dist_x + dist_y * dist_y);

      if (dist_xy < result(i, 0)) {

        result(i, 0) = dist_xy;

        result(i, 1) = j + 1;

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

foo_rcpp <- function(){rcpp_calc_dist_fish(as.matrix(fish_population[, c(3,4)]), coords_reef)}
foo_r <- function(){int_calc_dist_reef(fish_population[, c(3, 4)], coords_reef)}

bench::mark(foo_rcpp(), foo_r(),
            check = FALSE, relative = TRUE, iterations = 1000)
*/
