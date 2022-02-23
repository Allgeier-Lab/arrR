#include <Rcpp.h>

#include "rcpp_move_rand.h"
#include "rcpp_closest_reef.h"
#include "rcpp_modify_degree.h"
#include "rcpp_rlognorm.h"
#include "rcpp_translate_torus.h"
#include "rcpp_update_coords.h"

using namespace Rcpp;

//' rcpp_move_rand
//'
//' @description
//' Rcpp simulate movement (rand/attr).
//'
//' @param fishpop Matrix with fishpop values.
//' @param move_mean,move_var Double with mean and variance movement parameter.
//' @param max_dist Numeric with maximum movement distance
//' @param reef_attraction Bool if attracted towards reef.
//' @param coords_reef Matrix with ID and coords of reef cells.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Simulate movement of indivudals eiter either random (\code{reef_attraction = FALSE})
//' or attracted towards the reef cells (\code{reef_attraction = TRUE}).
//'
//' In the case of random movement, each time step a random movement distance
//' is drawn from a lognorm distribution and the individal moves into a random heading
//' direction drawn from an uniform distribution.
//'
//' In the case of attracted movement, fish individuals are aware of the distance to
//' the closest reef cell in three directions ahead of them (-45, 0, 45 degree) and
//' always move in the direction of the shortest distance to a reef cell.
//'
//' @return void
//'
//' @aliases rcpp_move_rand
//' @rdname rcpp_move_rand
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_move_rand(Rcpp::NumericMatrix fishpop, double move_mean, double move_var,
                    double max_dist, bool reef_attraction, Rcpp::NumericMatrix coords_reef,
                    Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions) {

  // loop through fishpop individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    // sample move dist
    double move_dist = rcpp_rlognorm(move_mean, std::sqrt(move_var), 0.0, max_dist);

    // move towards reef
    if (reef_attraction) {

      // vector with direction angles
      Rcpp::NumericVector angles = Rcpp::NumericVector::create(-45.0, 0.0, 45.0);

      // create vector for 3 distances (left, straight, right)
      Rcpp::NumericVector distance(angles.length(), 0.0);

      // loop through all distances
      for (int j = 0; j < distance.length(); j++) {

        // get coordinate of Visibility coords
        // MH: This used to be move_visibility insead of move_dist
        double x_temp = fishpop(i, 2) +
          (move_dist * cos(std::fmod((fishpop(i, 4) + angles[j]), 360) * (M_PI / 180)));

        double y_temp = fishpop(i, 3) +
          (move_dist * sin(std::fmod((fishpop(i, 4) + angles[j]), 360) * (M_PI / 180)));

        // make sure coords are inside extent
        Rcpp::NumericVector coords_temp = rcpp_translate_torus(x_temp, y_temp, extent);

        // get closest reef distance
        distance[j] = rcpp_closest_reef(coords_temp[0], coords_temp[1], coords_reef)[1];

      }

      // left distance is smaller than straight and right
      if ((distance[0] < distance[1]) && (distance[0] < distance[2])) {

        fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), -45.0);

        // right distance is smaller than straight and left
      } else if ((distance[2] < distance[1]) && (distance[2] < distance[0])) {

        fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), 45.0);

      }
    }

    // update fish coordinates and activtity
    rcpp_update_coords(fishpop, i, move_dist, max_dist, extent);

  }
}
