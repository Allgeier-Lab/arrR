#include "rcpp_move_fishpop.h"
#include "rcpp_rlognorm.h"
#include "rcpp_translate_torus.h"
#include "rcpp_cell_from_xy.h"
#include "rcpp_modify_degree.h"

//' rcpp_move_fishpop
//'
//' @description Rcpp move fish population
//'
//' @param fishpop Matrix with fishpop values.
//' @param reef_dist Vector with distance to reef of each cell.
//' @param pop_mean_move,pop_var_move Double with mean and variance movement parameter.
//' @param pop_visibility Double with "sight" distance of fish.
//' @param max_dist Numeric with maximum movement distance
//' @param reef_attraction Bool if attracted towards reef.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Rcpp implementation to move fish individuals depending on move distance and
//' heading value.
//'
//' @return void
//'
//' @aliases rcpp_move_fishpop
//' @rdname rcpp_move_fishpop
//'
//' @export
// [[Rcpp::export]]
void rcpp_move_fishpop(Rcpp::NumericMatrix fishpop, Rcpp::NumericVector reef_dist,
                       double pop_mean_move, double pop_var_move, double pop_visibility,
                       double max_dist, bool reef_attraction,
                       Rcpp::NumericVector extent, Rcpp::NumericVector dimensions) {

  // loop through fishpop individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    // sample move dist
    double move_dist = rcpp_rlognorm(pop_mean_move,
                                     std::sqrt(pop_var_move),
                                     0, max_dist);

    // move towards reef
    if (reef_attraction) {

      // create matrix with 3 rows (left, straight, right) and 2 cols (x,y)
      Rcpp::NumericMatrix headings(3, 2);

      // create vector for distances
      Rcpp::NumericVector distance(3);

      // get coordinates within visibility left
      headings(0, 0) = fishpop(i, 2) +
        (pop_visibility * cos(std::fmod((fishpop(i, 4) + -45), 360) * (PI / 180)));

      headings(0, 1) = fishpop(i, 3) +
        (pop_visibility * sin(std::fmod((fishpop(i, 4) + -45), 360) * (PI / 180)));

      // get coordinates within visibility straight
      headings(1, 0) = fishpop(i, 2) +
        (pop_visibility * cos(fishpop(i, 4) * (PI / 180)));

      headings(1, 1) = fishpop(i, 3) +
        (pop_visibility * sin(fishpop(i, 4) * (PI / 180)));

      // get coordinates within visibility right
      headings(2, 0) = fishpop(i, 2) +
        (pop_visibility * cos(std::fmod((fishpop(i, 4) + 45), 360) * (PI / 180)));

      headings(2, 1) = fishpop(i, 3) +
        (pop_visibility * sin(std::fmod((fishpop(i, 4) + 45), 360) * (PI / 180)));

      // loop through all possible headings
      for (int j = 0; j < headings.nrow(); j++) {

        // use torus translation to make sure within enviornment
        headings(j, _) = rcpp_translate_torus(headings(j, _), extent);

        // get cell id of heading (remove one because c++ indexing)
        int cell_id = rcpp_cell_from_xy(headings(j, _), dimensions, extent) - 1;

        // get distance of heading
        distance(j) = reef_dist(cell_id);

      }

      // left distance is smaller than straight and right
      if ((distance(0) < distance(1)) & (distance(0) < distance(2))) {

        fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), -45.0);

      // right distance is smaller than straight and left
      } else if ((distance(2) < distance(1)) & (distance(2) < distance(0))) {

        fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), 45.0);

      }
    }

    // calculate new x coord
    NumericVector xy_temp = NumericVector::create(
      fishpop(i, 2) + (move_dist * cos(fishpop(i, 4) * (M_PI / 180.0))),
      fishpop(i, 3) + (move_dist * sin(fishpop(i, 4) * (M_PI / 180.0)))
    );

    // make sure coords are within study area
    xy_temp = rcpp_translate_torus(xy_temp, extent);

    // update x coord
    fishpop(i, 2) = xy_temp(0);

    // update y coord
    fishpop(i, 3) = xy_temp(1);

    // update activity -> 12.5 is maximum distance
    fishpop(i, 9) = (1 / max_dist) * move_dist + 1;

    // turn fish randomly after moving (runif always returns vector, thus (0))
    // MH: This could be correlated to heading; runif(min = heading - x, max = heading + x)
    fishpop(i, 4) = Rcpp::runif(1, 0.0, 360.0)(0);

  }
}

/*** R
# calculate new coordinates and activity
rcpp_move_fishpop(fishpop = fishpop_values,
                  reef_dist = seafloor_values[, "reef_dist"],
                  pop_mean_move = parameters$pop_mean_move,
                  pop_var_move = parameters$pop_var_move,
                  pop_visibility = parameters$pop_visibility,
                  max_dist = max_dist,
                  extent = as.vector(extent, mode = "numeric"),
                  dimensions = dimensions,
                  reef_attraction = reef_attraction)
*/
