#include "rcpp_move_fishpop.h"
#include "rcpp_translate_torus.h"
#include "rcpp_cell_from_xy.h"
#include "rcpp_modify_degree.h"
#include "rcpp_rlognorm.h"

//' rcpp_move_fishpop
//'
//' @description Rcpp move fish population
//'
//' @param fishpop Matrix with fishpop values.
//' @param reef_dist Vector with distance to reef of each cell.
//' @param move_mean Double with mean movement parameter.
//' @param pop_visibility Double with "sight" distance of fish.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//' @param move_mean Numeric with parameter.
//' @param pop_thres_reserves Vector with threshold of pop_max_reserves to drain prior to foraging
//' @param move_reef Double with mean movement distance when sheltering at reef
//' @param move_return Double with mean movement distance when returning to reef
//'
//' @details
//' Rcpp implementation to move fish individuals depending on move distance and
//' heading value.
//' "KSM": notes on code added
//' "Q": questions for Max
//'
//' @return void
//'
//' @aliases rcpp_move_fishpop
//' @rdname rcpp_move_fishpop
//'
//' @export
// [[Rcpp::export]]
void rcpp_move_fishpop(Rcpp::NumericMatrix fishpop, Rcpp::NumericVector reef_dist,
                       Rcpp::NumericMatrix coords_reef,
                       Rcpp::NumericVector pop_thres_reserves,
                       double move_mean, double move_reef, double move_return,
                       Rcpp::NumericVector extent, Rcpp::NumericVector dimensions) {

  // MH: No need to add double bearing as function argument. But we will need coords_reef (matrix) as argument

  // loop through fishpop individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    Rcout << std::endl << "i: " << i << std::endl;

    // init move_dist
    double move_dist = -1.0;

    // init matrix for temp coords
    Rcpp::NumericMatrix coords_temp(1, 2);

    // get current x,y coords
    coords_temp(0, 0) = fishpop(i, 2);

    coords_temp(0, 1) = fishpop(i, 3);

    // get cell id of current x,y coords
    int cell_id = rcpp_cell_from_xy(coords_temp, dimensions, extent) - 1;

    // init reef_dist_temp which is shortest distance to reef
    double reef_dist_temp = R_PosInf;

    // init id of closest reef cell
    int reef_dist_id = -1;

    // calculate distance between current coords and closest reef cell
    for (int i = 0; i < coords_reef.nrow(); i++) {

      // calculate distance in x direction
      double dist_x = coords_temp(0, 0) - coords_reef(i, 0);

      // calculate distance in y direction
      double dist_y = coords_temp(0, 1) - coords_reef(i, 1);

      // calculate final distance
      double dist_xy = std::sqrt(std::pow(dist_x, 2.0) + std::pow(dist_y, 2.0));

      // check if current distance is smaller
      if (dist_xy < reef_dist_temp) {

        // update shortest distance
        reef_dist_temp = dist_xy;

        // update id
        reef_dist_id = i;

      }
    }

    // KSM: check if reserves are greater than x% (pop_thres_reserves) of reserves_max,
    // behaviour 1 and 2: reserves above doggy bag
    if (fishpop(i, 7) >= (pop_thres_reserves(i) * fishpop(i, 8))) {

      Rcout << "Behaviour 1 or 2" << std::endl;

      // behaviour 1: fish already at reef so they stay there
      if (reef_dist_temp <= 4.0) {

        Rcout << "Behaviour 1: Fish shelter at reef" << std::endl;

        //Behavior column = 1
        fishpop(i, 13) = 1.0;

        // KSM: move_dist is now from a log-normal distribution within 2m of reef to move
        move_dist = rcpp_rlognorm(move_reef, 1.0);

        Rcout << "move_dist L82: " << move_dist << std::endl;

        Rcout << "reef_dist L84: " << reef_dist_temp << std::endl;

        // turn fish randomly after moving (runif always returns vector, thus (0))
        fishpop(i, 4) = Rcpp::runif(1, 0.0, 360.0)(0);

      // behaviour 2: fish return towards reef
      } else {

        Rcout << "Behaviour 2" << std::endl;

        //Behavior column = 2
        fishpop(i, 13) = 2.0;

        //KSM: Here is where I need to calculatue reef_dist_temp based on most direct line to reef

        // MH: YES!
        // bearing between fish xy (coords_temp(0,0) coords_temp(0,1)) and
        // reef xy (coords_reef(reef_dist_id, 0), coords_reef(reef_dist_id, 1))

        // save bearing into heading fishpop(i, 4)

        // MH: old code which could be useful

        // Compute bearing between fish in degrees and reef cell in degrees
        // these are given in screen coordinates (?)
        // Q: this is from stackexchange. this creates a 'bearing' function - is line 183 necessary?
        // MH: Not sure what line 183 but here are many problems I think. Do you try to
        // define a function within a function? That wont work. Either create a complete new
        // function in a new script to calculate bearing and call this here or use only
        // "body" (so the code) of the function here.

        //double bearing(fishpop(i,3), fishpop(i,2), coords_reef(i,1), coords_reef(i,0)); {

        // MH: We could define this at the top of the script since no need to do this each
        // iteration of the loop
          // static const double two_pi = 6.2831853071795865;

          // double theta = atan2(coords_reef(i,1) - fishpop(i,3), fishpop(i,2) - coords_reef(i,0));

        // correct for 4 quadrants of coordinate system
          // if (theta < 0.0) {

          // return theta += two_pi; }

        //}

        // now this needs to be the heading to calculate reef_dist_temp before the next if,else statement
        // Q: I am not sure how to do this
        // MH: Not sure what you are asking here?

        // KSM: check if move_return is less than distance to reef
        // fish are further away from reef than move_mean
        // KSM: need to update line numbers

        if (move_return <= reef_dist_temp) {

          move_dist = rcpp_rlognorm(move_return, 1.0);

          Rcout << "Behaviour 2: Fish are far away" << std::endl;

          Rcout << "move_dist L162: " << move_dist << std::endl;

          Rcout << "reef_dist L164: " << reef_dist_temp << std::endl;

        // KSM: move_return is greater than distance to reef, travel a distance less than reef_dist_temp
        } else {

          // pull move_dist from log norm of distance to reef
          // this line does not seem to be working - fish not ending on reef
          move_dist = rcpp_rlognorm(reef_dist_temp, 1.0);

          Rcout << "Behaviour 2: Fish are close" << std::endl;

          Rcout << "move_dist L172: " << move_dist << std::endl;

          Rcout << "reef_dist L178: " << reef_dist_temp << std::endl;

        }
      }

    // KSM: behavior 3 - foraging
    // KSM: we want fish to move randomly, based on move_mean
    } else {

      Rcout << "Behaviour 3: Forage randomly" << std::endl;

      //Behavior column = 3
      fishpop(i, 13) = 3.0;

      // pull move_dist from log norm with mean_move
      move_dist = rcpp_rlognorm(move_mean, 1.0);

      Rcout << "move_dist L210: " << move_dist << std::endl;

      // turn fish randomly after moving (runif always returns vector, thus (0))
      // KSM: this needs to be moved within only behavior 1 and 3
      fishpop(i, 4) = Rcpp::runif(1, 0.0, 360.0)(0);

    }

    // MH: this can be deleted later, just safety check
    if (move_dist == -1.0) {

      stop("move_dist is zero...which it shouldn't!");


    }
    // Q: does this need to be moved to be only
    // calculate new x coord
    NumericVector xy_temp = NumericVector::create(
      fishpop(i, 2) + (move_dist * cos(fishpop(i, 4) * (M_PI / 180.0))),
      fishpop(i, 3) + (move_dist * sin(fishpop(i, 4) * (M_PI / 180.0)))
    );

    Rcout << "Start final torus" << std::endl;

    Rcout << "xy_temp: " << xy_temp << std::endl;
    Rcout << "extent: " << extent << std::endl;

    // make sure coords are within study area
    xy_temp = rcpp_translate_torus(xy_temp, extent);

    Rcout << "Update xy" << std::endl;

    // update x coord
    fishpop(i, 2) = xy_temp(0);

    // update y coord
    fishpop(i, 3) = xy_temp(1);

    Rcout << "Update ACT" << std::endl;

    // update activity
    fishpop(i, 9) = (1 / (move_mean + 1)) * move_dist + 1;

    Rcout << "DONE!" << std::endl;

  }
}

/*** R

# calculate new coordinates and activity
rcpp_move_fishpop(fishpop = fishpop_values,
                  reef_dist = seafloor_values[, "reef_dist"],
                  move_mean = parameters$move_mean,
                  pop_visibility = parameters$pop_visibility,
                  pop_thres_reserves = pop_thres_reserves,
                  move_reef = parameters$move_reef,
                  move_return = parameters$move_return,
                  extent = extent,
                  dimensions = dimensions,
                  behavior = behavior)
*/
