#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_translate_torus
//'
//' @description Rcpp translate torus
//'
//' @param coords Matrix with coordinates.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//'
//' @details
//' Rcpp implementation to translate coordinates if they exceed extent.
//' "KSM" notes from Katrina to help understand code
//' "Q" questions Katrina has for Max
//' "C" code to add
//' @return void
//'
//' @aliases rcpp_translate_torus
//' @rdname rcpp_translate_torus
//'
//' @keywords export
// [[Rcpp::export]]
void rcpp_translate_torus(Rcpp::NumericMatrix coords,
                          Rcpp::NumericVector extent) {

  // KSM: loop through all cells?
  for (int i = 0; i < coords.nrow(); i++) {

    // Q: what is this doing exactly? idenitfying x,y coord for each cell?
    // translate x coords left side
    while (coords(i, 0) < extent(0)) {

      coords(i, 0) = extent(1) - (extent(0) - coords(i, 0));

    }

    // translate x coord right side
    while (coords(i, 0) > extent(1)) {

      coords(i, 0) = extent(0) + (coords(i, 0) - extent(1));

    }

    // translate y coord bottom
    while (coords(i, 1) < extent(2)) {

      coords(i, 1) = extent(3) - (extent(2) - coords(i, 1));

    }

    // translate y coord top
    while (coords(i, 1) > extent(3)) {

      coords(i, 1) = extent(2) + (coords(i, 1) - extent(3));

    }
  }
}

//' rcpp_modify_degree
//'
//' @description Rcpp modify degree
//'
//' @param x Numeric with current angle in degree.
//' @param y Numerich with change of degree (negative or positive).
//'
//' @details
//' Rcpp implementation to substract or add degree to angle. Makes sure angles are
//' between 0 <= x <= 360.
//'
//' @return double
//'
//' @aliases rcpp_modify_degree
//' @rdname rcpp_modify_degree
//'
//' @keywords export
// [[Rcpp::export]]
double rcpp_modify_degree(double x, double y) {

  // add value to degree
  x += y;

  // get reminder of division
  // KSM: divide 360 from x coord
  x = std::fmod(x, 360);

  // if x < 0, result will be negative
  if (x < 0) {

    x += 360;

  }

  return(x);
}

//' rcpp_turn_fish
//'
//' @description Rcpp turn fish
//'
//' @param fishpop Matrix with fishpop values.
//' @param dist_values Matrix with distance to reef values (left, straight, right).
//'
//' @details
//' Rcpp implementation to turn fish individuals either left (-45°), straight (0°) or
//' right (45°) depending on which directions minimizes distance to reef.
//'
//' @return void
//'
//' @aliases rcpp_turn_fish
//' @rdname rcpp_turn_fish
//'
//' @keywords export
// [[Rcpp::export]]
void rcpp_turn_fish(Rcpp::NumericMatrix fishpop,
                    Rcpp::NumericMatrix dist_values) {

  // KSM: loop through fishpop individuals
  // Q: why is this different from fishpop_growth: fish_id.length?
  for (int i = 0; i < fishpop.nrow(); i++) {

    // left distance is smaller than straight and right
    if ((dist_values(i, 0) < dist_values(i, 1)) & (dist_values(i, 0) < dist_values(i, 2))) {

      fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), -45.0);

    // right distance is smaller than straight and left
    } else if ((dist_values(i, 2) < dist_values(i, 1)) & (dist_values(i, 2) < dist_values(i, 0))) {

      fishpop(i, 4) = rcpp_modify_degree(fishpop(i, 4), 45.0);

    // straight distance is shorther than left and right
    } else {

      continue;

    }
  }
}

//' rcpp_move_fishpop
//'
//' @description Rcpp move fish population
//'
//' @param fishpop Matrix with fishpop values.
//' @param move_dist Vector with move distance of fish individuals.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param pop_mean_move Numeric with parameter.
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
void rcpp_move_fishpop(Rcpp::NumericMatrix fishpop, Rcpp::NumericVector move_dist,
                       Rcpp::NumericVector extent, double pop_mean_move) {

  // loop through fishpop individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    // init matrix for temp coords
    NumericMatrix xy_temp (1, 2);

    // create new x and y coordinates
    xy_temp(0, 0) = fishpop(i, 2) + (move_dist(i) * cos(fishpop(i, 4) * (M_PI / 180.0)));

    xy_temp(0, 1) = fishpop(i, 3) + (move_dist(i) * sin(fishpop(i, 4) * (M_PI / 180.0)));

    // make sure coords are within study area
    // KSM: checks extent from first rcpp implementation (top of script)
    rcpp_translate_torus(xy_temp, extent);

    // update values
    // KSM: x coord values
    fishpop(i, 2) = xy_temp(0, 0);

    // KSM: y coord values
    fishpop(i, 3) = xy_temp(0, 1);

    // turn fish randomly after moving (returns vector)
    // MH: This could be correlated to heading; runif(min = heading - x, max = heading + x)
    // Q: what do you mean in that comment ^^?
    // KSM: create random number (vector) for heading between 1-360
    fishpop(i, 4) = Rcpp::runif(1, 0.0, 360.0)(0);

    // update activity
    // KSM: activity = 1/mean_movement value +1) * move_dist(i) + 1
    // Q: how did you come up with this equation to calculate activity?
    fishpop(i, 9) = (1 / (pop_mean_move + 1)) * move_dist(i) + 1;

  }
}

/*** R
rcpp_translate_torus(x_coord = 5.25, y_coord = -5.5,
                     extent = extent)

add_degree(fishpop_values[22, "heading"], 45.0)
((fishpop_values[22, "heading"] + 45) %% 360)

rcpp_turn_fish(fishpop = fishpop_values,
               dist_values = dist_values)

rcpp_move_fishpop(fishpop = fishpop_values, extent = extent,
                  move_dist = move_dist,
                  pop_mean_move = parameters$pop_mean_move)
*/
