#include "rcpp_move_fishpop.h"
#include "rcpp_translate_torus.h"
#include "rcpp_cell_from_xy.h"
#include "rcpp_modify_degree.h"

//' rcpp_move_fishpop
//'
//' @description Rcpp move fish population
//'
//' @param fishpop Matrix with fishpop values.
//' @param reef_dist Vector with distance to reef of each cell.
//' @param move_dist Vector with move distance of fish individuals.
//' @param move_mean Double with mean movement parameter.
//' @param pop_visibility Double with "sight" distance of fish.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//' @param move_mean Numeric with parameter.
//' @param pop_thres_reserves Vector with threshold of pop_max_reserves to drain prior to foraging
//' @param move_reef Double with mean movement distance when sheltering at reef
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
                       Rcpp::NumericVector move_dist, Rcpp::NumericVector pop_thres_reserves,
                       double move_mean, double move_reef,
                       double pop_visibility,
                       Rcpp::NumericVector extent, Rcpp::NumericVector dimensions) {

    // loop through fishpop individuals
    for (int i = 0; i < fishpop.nrow(); i++) {

      // KSM: check if reserves are greater than or equal to 10% of max_reserves (pop_thres_reserves)
      // Q: OK is this how I would write it?
      // MH: I am a little bit confused. Do you want to calculate proportion of reserves or should this
      // be a parameter ?
      // MH: Also, we probably need to wrap this in std::exp(), right?
      // MH: Watch out, rlnorm returns a vector even if only one number is needed
      // KSM: MOVE THIS TO RUN_SIMULATION
      double pop_thres_reserves = Rcpp::rlnorm(1, 0.1, 1.0)(0);

      // behaviour 1 and 2: reserves above doggy bag
      if fish_pop(i, 7) >= pop_thres_reserves * fishpop(i, 8) {

        // MH: I would suggest to rather check if reef_dist of that cell is below e.g. 2m

        // MH: get cell id of current location (make sure this acutally works :D)
        int cell_id = rcpp_cell_from_xy(fishpop(i, Range(1, 2)), dimensions, extent) - 1;

        // behaviour 1: fish already at reef so they stay there
        if (reef_dist(cell_id) <= 2.0) {

        // KSM: Create log-normal distribution within 2m (or some distance) of reef to move

        double move_dist = Rcpp::rlnorm(1, move_reef, 1.0)(0);

          // Q: now how would I make their movement be this?
          // MH: See line 171 but only use move_reef, we will get rid of move dist
          // move_dist == move_reef

        // behaviour 2: fish return towards reef
        } else {

          // KSM: fish checks surroundings. we might give them a bit "more knowledge" at one point

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

          // check which direction has shortest distance to reef
          // left direction is shortest
          if ((distance(0) < distance(1)) & (distance(0) < distance(2))) {

            fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), -45.0);

            // save distance to check with move_mean
            double reef_dist_temp = distance(0);

            // right distance is smaller than straight and left
            // Q: what does "else if" do?
            // MH: so if (x==y) {A} else {B} checks if x equals y and if yes does A and
            // if not does B. Use else if, allows in case the first if was false to
            // acutally check another conidtion

          // right direction is shortest
          } else if ((distance(2) < distance(1)) & (distance(2) < distance(0))) {

            fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), 45.0);

            // save distance to check with move_mean
            double reef_dist_temp = distance(2);

          // straight direction is shortest
          } else {

            // save distance to check with move_mean
            double reef_dist_temp = distance(2);

          }

          // KSM: check if mean_move is less than distance to reef
          // Q: should these be written as the column #, not the parameter?
          // Q: need help starting from here

          // MH: Okay, I think here is a big mess and you are in the wrong loop.
          // The loop starting in l121 just gets the distance to reef for all three
          // directions. I think we need to first run the loop without any changes.
          // Then, starting in line 160 (if distance(0) etc.) we need to check which
          // direction is the shortest towards the reef and then use this distance to
          // check againgst move_mean

          // fish are further away from reef than move_mean
          // KSM: HELP HERE
          if (move_mean <= reef_dist_temp) {

          // KSM: move based on move_mean
          // MH: Pull move dist from move_mean log norm
          // double move_dist =

          // KSM: if move_mean is greater than distance to reef, pull from limited distance distribution (less than pop_mean_move)
          // Q: how should we write this? Do you have any ideas? So we want move_mean to be shorter to not overshoot reef
          } else {


            // pull move_dist from log norm where move_mean < reef_dist_temp
            // double move_dist =

          }
        }
      }

      // KSM: Behavior 3 - foraging
      // KSM: we want fish to move randomly, based on move_mean
      } else {

        // pull move_dist from log norm with mean_moce
        // double move_dist =


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

      // update activity
      fishpop(i, 9) = (1 / (move_mean + 1)) * move_dist + 1;

      // turn fish randomly after moving (runif always returns vector, thus (0))
      // MH: This could be correlated to heading; runif(min = heading - x, max = heading + x)
      fishpop(i, 4) = Rcpp::runif(1, 0.0, 360.0)(0);

}

/*** R

# calculate new coordinates and activity
rcpp_move_fishpop(fishpop = fishpop_values,
                  reef_dist = seafloor_values[, "reef_dist"],
                  move_dist = move_dist,
                  move_mean = parameters$move_mean,
                  pop_visibility = parameters$pop_visibility,
                  pop_thres_reserves = parameters$pop_thres_reserves,
                  move_reef = parameters$move_reef,
                  extent = extent,
                  dimensions = dimensions)
*/
