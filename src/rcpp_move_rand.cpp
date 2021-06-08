#include "rcpp_move_rand.h"
#include "rcpp_closest_reef.h"
#include "rcpp_modify_degree.h"
#include "rcpp_rlognorm.h"
#include "rcpp_translate_torus.h"
#include "rcpp_update_coords.h"

//' rcpp_move_rand
//'
//' @description Rcpp move random
//'
//' @param fishpop Matrix with fishpop values.
//' @param coords_reef Matrix with ID and coords of reef cells.
//' @param move_mean,move_var Double with mean and variance movement parameter.
//' @param move_visibility Double with "sight" distance of fish.
//' @param max_dist Numeric with maximum movement distance
//' @param reef_attraction Bool if attracted towards reef.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Rcpp implementation to move fish individuals randomly or attracted depending
//' on move distance and heading value.
//'
//' @references
//' Add reference
//'
//' @return void
//'
//' @aliases rcpp_move_rand
//' @rdname rcpp_move_rand
//'
//' @export
// [[Rcpp::export]]
void rcpp_move_rand(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                    double move_mean, double move_var, double move_visibility,
                    double max_dist, bool reef_attraction,
                    Rcpp::NumericVector extent, Rcpp::NumericVector dimensions) {

  // loop through fishpop individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    // sample move dist
    double move_dist = rcpp_rlognorm(move_mean, std::sqrt(move_var), 0, max_dist);

    // move towards reef
    if (reef_attraction) {

      // vector with direction angles
      NumericVector angles = NumericVector::create(-45.0, 0.0, 45.0);

      // create vector for 3 distances (left, straight, right)
      Rcpp::NumericVector distance(angles.length(), 0.0);

      // loop through all distances
      for (int j = 0; j < distance.length(); j++) {

        // MH: This could be move_dist instead of move_visibility
        double x_temp = fishpop(i, 2) +
          (move_visibility * cos(std::fmod((fishpop(i, 4) + angles(j)), 360) * (PI / 180)));

        double y_temp = fishpop(i, 3) +
          (move_visibility * sin(std::fmod((fishpop(i, 4) + angles(j)), 360) * (PI / 180)));

        NumericVector coords_temp = NumericVector::create(x_temp, y_temp);

        coords_temp = rcpp_translate_torus(coords_temp, extent);

        distance(j) = rcpp_closest_reef(coords_temp, coords_reef)(1);

      }

      // left distance is smaller than straight and right
      if ((distance(0) < distance(1)) && (distance(0) < distance(2))) {

        fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), -45.0);

        // right distance is smaller than straight and left
      } else if ((distance(2) < distance(1)) && (distance(2) < distance(0))) {

        fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), 45.0);

      }
    }

    // update fish coordinates and activtity
    rcpp_update_coords(fishpop, i, move_dist, max_dist, extent);

  }
}

/*** R
# calculate new coordinates and activity
rcpp_move_rand(fishpop = fishpop_values,
               coords_reef = coords_reef,
               move_mean = parameters$move_mean,
               move_var = parameters$move_var,
               move_visibility = parameters$move_visibility,
               max_dist = max_dist,
               extent = as.vector(extent, mode = "numeric"),
               dimensions = dimensions,
               reef_attraction = reef_attraction)
*/
