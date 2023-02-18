#include <Rcpp.h>

#include "rcpp_move_behav.h"
#include "rcpp_which.h"
#include "rcpp_closest_reef.h"
#include "rcpp_get_bearing.h"
#include "rcpp_rlognorm.h"
#include "rcpp_update_coords.h"

using namespace Rcpp;

//' rcpp_move_behav
//'
//' @description
//' Rcpp simulate movement (behav).
//'
//' @param fishpop Matrix with fishpop values.
//' @param fishpop_attr Matrix with id and threshold of pop_reserves_max.
//' @param move_mean,move_sd Vector with mean movement parameter.
//' @param move_reef Vector with mean movement distance when sheltering at reef.
//' @param move_border Vector with movement distance that surrounds reef cell border.
//' @param move_return Vector with mean movement distance when returning to reef.
//' @param max_dist Vector with maximum movement distance
//' @param coords_reef Matrix with ID and coords of reef cells.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Fish individuals move based on how much nutrients they have stored in their
//' reserves. There are three different movement states.
//'
//' If reserves are above a certain threshold, individuals either shelter at reef cells
//' (state 1) or move back towards reef cells (state 2). If reserves are not above
//' the threshold, individuals move randomly across the seafloor to forage (state 3).
//'
//' @return void
//'
//' @aliases rcpp_move_behav
//' @rdname rcpp_move_behav
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_move_behav(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_attr,
                     Rcpp::NumericVector move_mean, Rcpp::NumericVector move_sd, Rcpp::NumericVector move_reef,
                     Rcpp::NumericVector move_border, Rcpp::NumericVector move_return, NumericVector max_dist,
                     Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector extent,
                     Rcpp::IntegerVector dimensions, Rcpp::NumericVector behavior) {

  // loop through fishpop individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    // get current species id
    int species_temp = fishpop(i, 1) - 1;

    // init move_dist and bearing
    double move_dist;

    // init vector for reef distance
    Rcpp::NumericVector closest_reef (2, 0.0);

    // get current row id
    int id_attr = rcpp_which(fishpop(i, 0), fishpop_attr(_, 0));

    // behavior 3: foraging
    if (fishpop(i, 12) == 3.0) {

      // keep foraging because reserves below maximum
      if (fishpop(i, 10) < fishpop(i, 11)) {

        // pull move_dist from log norm with mean_move
        // use behavior type to determine whether fish should leave reef to forage
        if (behavior[0] == 0) {
          move_dist = rcpp_rlognorm(move_mean[species_temp], move_sd[species_temp], 0.0, max_dist[species_temp]);
        } else {
          // still "on reef"
          if (closest_reef[1] <= move_border[species_temp]) {

            // pull random movement distance
            move_dist = rcpp_rlognorm(move_reef[species_temp], 1.0, 0.0, max_dist[species_temp]);

            // moved away from reef; switch to behavior 2
          } else {

            // update heading towards reef
            fishpop(i, 5) = rcpp_get_bearing(fishpop(i, 3), fishpop(i, 4),
                    coords_reef(closest_reef[0], 1),
                    coords_reef(closest_reef[0], 2));;

            // use either distance to closest_reef or move_return distance
            double move_temp = std::min(closest_reef[1], move_return[species_temp]);

            // sample move distance from around distance to reef
            move_dist = rcpp_rlognorm(move_temp, 1.0, 0.0, max_dist[species_temp]);

            fishpop(i, 12) = 2.0;

          }
        }
      // reserves are full, switch to behavior 2
      } else {

        fishpop(i, 12) = 2.0;

      }
    }

    // behavior 2: move towards reef
    if (fishpop(i, 12) == 2.0) {

      // reserves above threshold; return towards reef
      if (fishpop(i, 10) >= (fishpop_attr(id_attr, 1) * fishpop(i, 11))) {

        // get id and distance to closest reef
        closest_reef = rcpp_closest_reef(fishpop(i, 3), fishpop(i, 4), coords_reef);

        // reef is further away than the threshold to be "on the reef"
        if (closest_reef[1] > move_border[species_temp]) {

          // update heading towards reef
          fishpop(i, 5) = rcpp_get_bearing(fishpop(i, 3), fishpop(i, 4),
                                           coords_reef(closest_reef[0], 1),
                                           coords_reef(closest_reef[0], 2));;

          // use either distance to closest_reef or move_return distance
          double move_temp = std::min(closest_reef[1], move_return[species_temp]);

          // sample move distance from around distance to reef
          move_dist = rcpp_rlognorm(move_temp, 1.0, 0.0, max_dist[species_temp]);

        // already at reef, switch to behavior 1
        } else {

          fishpop(i, 12) = 1.0;

        }

      // start to forage because reserves below threshold, switch to behavior 3
      } else {

        // pull move_dist from log norm with mean_move
        move_dist = rcpp_rlognorm(move_mean[species_temp], move_sd[species_temp],
                                  0.0, max_dist[species_temp]);

        fishpop(i, 12) = 3.0;

      }
    }

    if (fishpop(i, 12) == 1.0) {

      // stay at reef because reserves are above threshold
      if (fishpop(i, 10) >= (fishpop_attr(id_attr, 1) * fishpop(i, 11))) {

        // get id and distance to closest reef
        closest_reef = rcpp_closest_reef(fishpop(i, 3), fishpop(i, 4), coords_reef);

        // still "on reef"
        if (closest_reef[1] <= move_border[species_temp]) {

          // pull random movement distance
          move_dist = rcpp_rlognorm(move_reef[species_temp], 1.0, 0.0, max_dist[species_temp]);

        // moved away from reef; switch to behavior 2
        } else {

          // update heading towards reef
          fishpop(i, 5) = rcpp_get_bearing(fishpop(i, 3), fishpop(i, 4),
                                           coords_reef(closest_reef[0], 1),
                                           coords_reef(closest_reef[0], 2));;

          // use either distance to closest_reef or move_return distance
          double move_temp = std::min(closest_reef[1], move_return[species_temp]);

          // sample move distance from around distance to reef
          move_dist = rcpp_rlognorm(move_temp, 1.0, 0.0, max_dist[species_temp]);

          fishpop(i, 12) = 2.0;

        }

      // start to forage because reserves below threshold, switch to behavior 3
      } else {

        // pull move_dist from log norm with mean_move
        move_dist = rcpp_rlognorm(move_mean[species_temp], move_sd[species_temp], 0.0, max_dist[species_temp]);

        fishpop(i, 12) = 3.0;

      }
    }

    // update fish coordinates and activity
    rcpp_update_coords(fishpop, i, move_dist, max_dist[species_temp], extent);
  }
}
