#include "rcpp_move_fishpop.h"
#include "rcpp_closest_reef.h"
#include "rcpp_rlognorm.h"
#include "rcpp_get_bearing.h"
#include "rcpp_translate_torus.h"

//' rcpp_move_fishpop
//'
//' @description Rcpp move fish population
//'
//' @param fishpop Matrix with fishpop values.
//' @param coords_reef Matrix with coords of reef cells.
//' @param pop_thres_reserves Vector with threshold of pop_max_reserves to drain prior to foraging.
//' @param move_border Double with movement distance that surrounds reef cell border.
//' @param move_mean Double with mean movement parameter.
//' @param move_reef Double with mean movement distance when sheltering at reef.
//' @param move_return Double with mean movement distance when returning to reef.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Rcpp implementation to move fish individuals depending on move distance and
//' heading value.
//' "KSM": notes on code added, "MH" Some comments/ideas Max
//' "Q": questions for Max
//'
//' @return void
//'
//' @aliases rcpp_move_fishpop
//' @rdname rcpp_move_fishpop
//'
//' @export
// [[Rcpp::export]]
void rcpp_move_fishpop(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                       Rcpp::NumericVector pop_thres_reserves,
                       double move_border, double move_mean, double move_reef, double move_return,
                       Rcpp::NumericVector extent, Rcpp::NumericVector dimensions) {

  // loop through fishpop individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    Rcout << std::endl << "i ; " << i << std::endl;

    // init move_dist
    double move_dist = -1.0;

    // init matrix for temp coords
    Rcpp::NumericMatrix coords_temp(1, 2);

    // get current x,y coords
    coords_temp(0, 0) = fishpop(i, 2);

    coords_temp(0, 1) = fishpop(i, 3);

    // get id and distance to closest reef
    Rcpp::NumericVector closest_reef = rcpp_closest_reef(coords_temp, coords_reef);

    // behaviour 1 and 2: reserves above doggy bag
    if (fishpop(i, 7) >= (pop_thres_reserves(i) * fishpop(i, 8))) {

      Rcout << "closest_reef ; " << closest_reef << std::endl;

      // behaviour 1: fish already at reef so they stay there
      // MH: What about to set this distance threshold to move_reef?
      if (closest_reef(1) <= move_border) {

        //Behavior column = 1
        fishpop(i, 13) = 1.0;

        // KSM: move_dist is now from a log-normal distribution within Xm of reef to move
        move_dist = rcpp_rlognorm(move_reef, 1.0);

        Rcout << "move_dist ; " << move_dist << std::endl;

        Rcout << "behavior ; " << fishpop(i, 13) << std::endl;

        // turn fish randomly (runif always returns vector, thus (0))
        fishpop(i, 4) = Rcpp::runif(1, 0.0, 360.0)(0);

      // behaviour 2: fish return towards reef
      } else {

        // set behavior column
        fishpop(i, 13) = 2.0;

        double theta = rcpp_get_bearing(coords_temp(0, 0),
                                        coords_temp(0, 1),
                                        coords_reef(closest_reef(0), 0),
                                        coords_reef(closest_reef(0), 1));

        // update heading
        fishpop(i, 4) = theta;

        // check if reef is further away then move return distance
        if (move_return <= closest_reef(1)) {

          // sample move distance from lognorm of move_return (swim faster/move further)
          move_dist = rcpp_rlognorm(move_return, 1.0);

          Rcout << "move_dist ; " << move_dist << std::endl;

          Rcout << "behavior ; " << fishpop(i, 13) << std::endl;

        // reef is closer than move_return, so make sure fish don't overshoot
        } else {

          // sample move distance from around distance to reef
          move_dist = rcpp_rlognorm(closest_reef(1), 1.0);

          Rcout << "move_dist ; " << move_dist << std::endl;

          Rcout << "behavior ; " << fishpop(i, 13) << std::endl;

        }
      }

    // behavior 3: foraging
    } else {

      // Behavior column = 3
      fishpop(i, 13) = 3.0;

      // pull move_dist from log norm with mean_move
      move_dist = rcpp_rlognorm(move_mean, 1.0);

      Rcout << "move_dist ; " << move_dist << std::endl;

      Rcout << "closest_reef ; " << closest_reef << std::endl;

      Rcout << "behavior ; " << fishpop(i, 13) << std::endl;

      // turn fish randomly after moving (runif always returns vector, thus (0))
      fishpop(i, 4) = Rcpp::runif(1, 0.0, 360.0)(0);

    }

    // MH: this can be deleted later, just safety check
    if (move_dist == -1.0) {

      stop("move_dist is zero...which it shouldn't!");

    }

    // calculate new xy coords using different distance and heading based on behavior
    Rcpp::NumericVector xy_temp = NumericVector::create(
      fishpop(i, 2) + (move_dist * cos(fishpop(i, 4) * (M_PI / 180.0))),
      fishpop(i, 3) + (move_dist * sin(fishpop(i, 4) * (M_PI / 180.0)))
    );

    // make sure coords are within study area
    xy_temp = rcpp_translate_torus(xy_temp, extent);

    // update x coord
    fishpop(i, 2) = xy_temp(0);

    // update y coord
    fishpop(i, 3) = xy_temp(1);

    // update activity
    fishpop(i, 9) = (1 / (move_return + 1)) * move_dist + 1;

  }
}

/*** R
# calculate new coordinates and activity
rcpp_move_fishpop(fishpop = fishpop_values,
                  coords_reef = coords_reef,
                  move_border = parameters$move_border,
                  move_mean = parameters$move_mean,
                  pop_thres_reserves = pop_thres_reserves,
                  move_reef = parameters$move_reef,
                  move_return = parameters$move_return,
                  extent = as.vector(extent, mode = "numeric"),
                  dimensions = dimensions)
*/
