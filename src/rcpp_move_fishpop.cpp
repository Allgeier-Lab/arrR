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
//' @param pop_mean_move Double with mean movement parameter.
//' @param pop_visibility Double with "sight" distance of fish.
//' @param reef_attraction Bool if attracted towards reef.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//' @param pop_mean_move Numeric with parameter.
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
                       Rcpp::NumericVector move_dist, double pop_mean_move,
                       double pop_visibility, bool reef_attraction,
                       Rcpp::NumericVector extent, Rcpp::NumericVector dimensions) {

  // KSM: will need to add in all new parameters (prop_reserves, reef_mean_move)

  // loop through fishpop individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

  // KSM: check if reserves are greater than or equal to 10% of max_reserves
  // KSM: here is where we will need to add a parameter instead of constant 10%
  // Q: in what R file do I write parameters?
  // Q: fish_id_temp does not seem to be in this C++ file - how do I write this in this .cpp file?

  if fish_pop(fish_id_temp, 7) >= 0.10 * fishpop(fish_id_temp, 8) {

    // KSM: check if fish is at reef
    // KSM: check x and y coords between fish and reef
    // Q: these coords need to only be coords with reef - do I need to add (seafloor(cell_id_temp, 2) = 1)?
    if fish_pop(fish_id_temp, 3) = seafloor(cell_id_temp, 0) &
      fish_pop(fish_id_temp, 4) = seafloor(cell_id_temp, 2)

    // KSM: Behavior 1 - shelter on reef
    // KSM: this is where I will add in a log-normal distribution within 2m (or some distance) of reef
    // Q: should I create a parameter similar to pop_mean_move that is reef_mean_move with some mean (2m) and SD?
    // Q: can you help me write a log-normal distribution in C++? I am not sure how to do this




    // KSM: Behavior 2 - return to reef
    // KSM: fish need to have knowledge of distance to reef
    } else {

    // Q: here I took your code from below to check surroundings/find reef. is this what we want to do?

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

      // KSM: check if pop_mean_move is less than distance to reef
      // Q: should these be written as the column #, not the parameter?
      if pop_mean_move <= reef_dist {

      // KSM: move based on pop_mean_move
      move_dist = pop_mean_move

      // KSM: if pop_mean_move is greater than distance to reef, pull from limited distance distribution (less than pop_mean_move)
      // Q: how should we write this? Do you have any ideas? So we want pop_mean_move to be shorter to not overshoot reef
      // Q: should it be pop_mean_move minus some constant or some proportion of pop_mean_move?
      } else {



      }

      // KSM: Behavior 3 - foraging
      // KSM: we want fish to move randomly, based on pop_mean_move
      } else {

      // Q: do we need this code here? since fish is foraging randomly on landscape
      // left distance is smaller than straight and right
      if ((distance(0) < distance(1)) & (distance(0) < distance(2))) {

        fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), -45.0);

      // right distance is smaller than straight and left
      // Q: what does "else if" do?
      } else if ((distance(2) < distance(1)) & (distance(2) < distance(0))) {

        fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), 45.0);

      }
    }

    // calculate new x coord
    NumericVector xy_temp = NumericVector::create(
      fishpop(i, 2) + (move_dist(i) * cos(fishpop(i, 4) * (M_PI / 180.0))),
      fishpop(i, 3) + (move_dist(i) * sin(fishpop(i, 4) * (M_PI / 180.0)))
    );

    // make sure coords are within study area
    xy_temp = rcpp_translate_torus(xy_temp, extent);

    // update x coord
    fishpop(i, 2) = xy_temp(0);

    // update y coord
    fishpop(i, 3) = xy_temp(1);

    // update activity
    fishpop(i, 9) = (1 / (pop_mean_move + 1)) * move_dist(i) + 1;

    // turn fish randomly after moving (runif always returns vector, thus (0))
    // MH: This could be correlated to heading; runif(min = heading - x, max = heading + x)
    fishpop(i, 4) = Rcpp::runif(1, 0.0, 360.0)(0);

  }
}

/*** R

# calculate new coordinates and activity
rcpp_move_fishpop(fishpop = fishpop_values,
                  reef_dist = seafloor_values[, "reef_dist"],
                  move_dist = move_dist,
                  pop_mean_move = parameters$pop_mean_move,
                  pop_visibility = parameters$pop_visibility,
                  extent = extent,
                  dimensions = dimensions,
                  reef_attraction = reef_attraction)
*/
