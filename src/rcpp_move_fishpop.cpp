#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_translate_torus
//'
//' @description Add describtion
//'
//' @param coords Add describtion
//' @param extent Add describtion
//'
//' @details
//' Add describtion
//'
//' @return Add describtion
//'
//' @aliases rcpp_translate_torus
//' @rdname rcpp_translate_torus
//'
//' @keywords export
// [[Rcpp::export]]
void rcpp_translate_torus(Rcpp::NumericMatrix coords,
                          Rcpp::NumericVector extent) {

  for (int i = 0; i < coords.nrow(); i++) {

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

//' rcpp_turn_fish
//'
//' @description Add describtion
//'
//' @param fishpop Add describtion
//' @param dist_values Add describtion
//'
//' @details
//' Add describtion
//'
//' @return Add describtion
//'
//' @aliases rcpp_turn_fish
//' @rdname rcpp_turn_fish
//'
//' @keywords export
// [[Rcpp::export]]
void rcpp_turn_fish(Rcpp::NumericMatrix fishpop,
                    Rcpp::NumericMatrix dist_values) {

  for (int i = 0; i < fishpop.nrow(); i++) {

    // left distance is smaller than straigth and rigth
    if (dist_values(i, 0) < dist_values(i, 1) & dist_values(i, 0) < dist_values(i, 2)) {

      fishpop(i, 4) -= 45.0;

    // right distance is smaller than straigth and left
    } else if (dist_values(i, 2) < dist_values(i, 1) & dist_values(i, 2) < dist_values(i, 0)) {

      fishpop(i, 4) += 45.0;

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
//' @param fishpop Add describtion
//' @param move_dist Add describtion
//' @param extent  Add describtion
//' @param pop_mean_move Add describtion
//'
//' @details
//' \code{Rcpp} implementation of to calculate growth.
//'
//' @return Matrix
//'
//' @aliases rcpp_move_fishpop
//' @rdname rcpp_move_fishpop
//'
//' @export
// [[Rcpp::export]]
void rcpp_move_fishpop(Rcpp::NumericMatrix fishpop, Rcpp::NumericVector move_dist,
                       Rcpp::NumericVector extent, double pop_mean_move) {

  for (int i = 0; i < fishpop.nrow(); i++) {

    // init matrix for temp coords
    NumericMatrix xy_temp (1, 2);

    // create new x and y coordinates
    xy_temp(0, 0) = fishpop(i, 2) + (move_dist(i) * cos(fishpop(i, 4) * (M_PI / 180.0)));

    xy_temp(0, 1) = fishpop(i, 3) + (move_dist(i) * sin(fishpop(i, 4) * (M_PI / 180.0)));

    // make sure coords are within study area
    rcpp_translate_torus(xy_temp, extent);

    // update values
    fishpop(i, 2) = xy_temp(0, 0);

    fishpop(i, 3) = xy_temp(0, 1);

    // turn fish randomly after moving (returns vector)
    // MH: This could be correlated to heading; runif(min = heading - x, max = heading + x)
    fishpop(i, 4) = Rcpp::runif(1, 0.0, 360.0)(0);

    // update activity
    fishpop(i, 9) = (1 / (pop_mean_move + 1)) * move_dist(i) + 1;

  }
}

/*** R
rcpp_translate_torus(x_coord = 5.25, y_coord = -5.5,
                     extent = extent)

rcpp_turn_fish(fishpop = fishpop_values,
               dist_values = dist_values)

rcpp_move_fishpop(fishpop = fishpop_values, extent = extent,
                  move_dist = move_dist,
                  pop_mean_move = parameters$pop_mean_move)
*/
