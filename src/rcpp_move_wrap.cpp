#include "rcpp_move_wrap.h"
#include "rcpp_move_rand.h"
#include "rcpp_move_behav.h"

//' rcpp_move_wrap
//'
//' @description
//' Rcpp movement behavior wrapper.
//'
//' @param fishpop Matrix with fishpop values.
//' @param coords_reef Matrix with ID and coords of reef cells.
//' @param movement String specifing movement algorithm. Either 'rand', 'attr' or 'behav'.
//' @param pop_thres_reserves Vector with threshold of pop_max_reserves to drain prior to foraging.
//' @param move_mean,move_var,move_visibility Double with mean movement parameter.
//' @param move_reef Double with mean movement distance when sheltering at reef.
//' @param move_border Double with movement distance that surrounds reef cell border.
//' @param move_return Double with mean movement distance when returning to reef.
//' @param max_dist Maximum distance an individual can move.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Wrapper function around different movement algorithms. Individuals can either move
//' completely random (\code{movement = "rand"}), attracted towards the artifical reef cells
//' \code{movement = "attr"} or movement can be  depending on the bioenergetics of the
//' fish individuals \code{movement = "behav"}. For more information see \code{\link{rcpp_move_rand}},
//' \code{\link{rcpp_move_rand}} or \code{\link{rcpp_move_behav}}.
//'
//' @return void
//'
//' @aliases rcpp_move_wrap
//' @rdname rcpp_move_wrap
//'
//' @export
// [[Rcpp::export]]
void rcpp_move_wrap(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                    String movement, Rcpp::NumericVector pop_thres_reserves,
                    double move_mean, double move_var, double move_visibility,
                    double move_reef, double move_border, double move_return, double max_dist,
                    Rcpp::NumericVector extent, Rcpp::NumericVector dimensions) {

  // random movement
  if (movement == "rand") {

    rcpp_move_rand(fishpop, coords_reef,
                   move_mean, move_var, move_visibility,
                   max_dist, FALSE, extent, dimensions);

    // attracted movement
  } else if (movement == "attr") {

    rcpp_move_rand(fishpop, coords_reef,
                   move_mean, move_var, move_visibility,
                   max_dist, TRUE, extent, dimensions);

    // behaviour movement
  } else if (movement == "behav") {

    rcpp_move_behav(fishpop, coords_reef, pop_thres_reserves,
                    move_mean, move_var, move_reef, move_border, move_return,
                    max_dist, extent, dimensions);

    // throw error
  } else {

    Rcpp::stop("'movement' must be either 'rand', 'attr', or 'behav'.");

  }
}

/*** R
rcpp_move_wrap(fishpop = fishpop_values, coords_reef = coords_reef, movement = movement,
               pop_thres_reserves = pop_thres_reserves,
               move_mean = parameters$move_mean, move_var = parameters$move_var, move_visibility = parameters$move_visibility,
               move_reef = parameters$move_reef, move_border = parameters$move_border, move_return = parameters$move_return,
               max_dist = max_dist, extent = extent, dimensions = dimensions)
*/
