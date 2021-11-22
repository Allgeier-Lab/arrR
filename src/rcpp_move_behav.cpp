#include <Rcpp.h>
#include "rcpp_move_behav.h"
#include "rcpp_closest_reef.h"
#include "rcpp_get_bearing.h"
#include "rcpp_rlognorm.h"
#include "rcpp_update_coords.h"

using namespace Rcpp;

//' rcpp_move_behav
//'
//' @description
//' Rcpp simulate movement based on bioenergetics.
//'
//' @param fishpop Matrix with fishpop values.
//' @param coords_reef Matrix with ID and coords of reef cells.
//' @param pop_reserves_thres Vector with threshold of pop_reserves_max to drain prior to foraging.
//' @param move_mean,move_var Double with mean movement parameter.
//' @param move_reef Double with mean movement distance when sheltering at reef.
//' @param move_border Double with movement distance that surrounds reef cell border.
//' @param move_return Double with mean movement distance when returning to reef.
//' @param max_dist Maximum distance an individual can move.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Fish indivivuals move based on how much nutrients they have stored in their
//' reserves. There are three different movement behaviors.
//'
//' If reservers above a certain threshold, individuals either shelter at reef cells (behavior 1)
//' or move back towards reef cells (behavior 2). If reserves are not above the threshold,
//' individuals move randomly across the environment to forage.
//'
//' @return void
//'
//' @aliases rcpp_move_behav
//' @rdname rcpp_move_behav
//'
//' @export
// [[Rcpp::export]]
void rcpp_move_behav(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                     Rcpp::NumericVector pop_reserves_thres,
                     double move_mean, double move_var,
                     double move_reef, double move_border, double move_return, double max_dist,
                     Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions) {

  // loop through fishpop individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    // init move_dist
    double move_dist = 0.0;

    // behaviour 1 and 2: reserves above doggy bag
    if (fishpop(i, 9) >= (pop_reserves_thres(i) * fishpop(i, 10))) {

      // get id and distance to closest reef
      Rcpp::NumericVector closest_reef = rcpp_closest_reef(fishpop(i, 2), fishpop(i, 3),
                                                           coords_reef);

      // behaviour 1: fish already at reef so they stay there
      if (closest_reef[1] <= move_border) {

        //Behavior column = 1
        fishpop(i, 11) = 1.0;

        // move_dist is now from a log-normal distribution within Xm of reef to move
        move_dist = rcpp_rlognorm(move_reef, 1.0, 0.0, max_dist);

      // behaviour 2: fish return towards reef
      } else {

        // set behavior column
        fishpop(i, 11) = 2.0;

        double theta = rcpp_get_bearing(fishpop(i, 2), fishpop(i, 3),
                                        coords_reef(closest_reef[0], 0),
                                        coords_reef(closest_reef[0], 1));

        // update heading
        fishpop(i, 4) = theta;

        // check if reef is further away then move return distance
        if (move_return <= closest_reef[1]) {

          // sample move distance from lognorm of move_return (swim faster/move further)
          move_dist = rcpp_rlognorm(move_return, 1.0, 0.0, max_dist);

        // reef is closer than move_return, so make sure fish don't overshoot
        } else {

          // sample move distance from around distance to reef
          move_dist = rcpp_rlognorm(closest_reef[1], 1.0, 0.0, max_dist);

        }
      }

    // behavior 3: foraging
    } else {

      // Behavior column = 3
      fishpop(i, 11) = 3.0;

      // pull move_dist from log norm with mean_move
      move_dist = rcpp_rlognorm(move_mean, std::sqrt(move_var), 0.0, max_dist);

    }

    // update fish coordinates and activtity
    rcpp_update_coords(fishpop, i, move_dist, max_dist, extent);

  }
}

/*** R
rcpp_move_behav(fishpop = fishpop_values, coords_reef = coords_reef,
                pop_reserves_thres = pop_reserves_thres, move_mean = parameters$move_mean,
                move_var = parameters$move_var, move_reef = parameters$move_reef,
                move_border = parameters$move_border, move_return = parameters$move_return,
                max_dist = max_dist, extent = as.vector(extent, mode = "numeric"),
                dimensions = dimensions)
*/
