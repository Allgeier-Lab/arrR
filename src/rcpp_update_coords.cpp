#include "rcpp_update_coords.h"
#include "rcpp_translate_torus.h"

//' rcpp_update_coords
//'
//' @description Rcpp move fish population
//'
//' @param fishpop Matrix with fishpop values.
//' @param i Integer with row id.
//' @param move_dist,max_dist Numeric with (maximum) movement distance.
//'
//' @details
//' Update xy coordinates and activity of fish individuals depending move_dist.
//'
//' @return void
//'
//' @aliases rcpp_update_coords
//' @rdname rcpp_update_coords
//'
//' @export
// [[Rcpp::export]]
void rcpp_update_coords(Rcpp::NumericMatrix fishpop, int i,
                        double move_dist, double max_dist, Rcpp::NumericVector extent) {

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
    fishpop(i, 9) = (1 / max_dist) * move_dist + 1;

    // turn fish randomly after moving (runif always returns vector, thus (0))
    // MH: This could be correlated to heading; runif(min = heading - x, max = heading + x)
    fishpop(i, 4) = Rcpp::runif(1, 0.0, 360.0)(0);

}

/*** R
update_coords(fishpop_values, i, move_dist, max_dist);
*/
