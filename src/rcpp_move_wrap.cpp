#include <Rcpp.h>

#include "rcpp_move_wrap.h"

#include "rcpp_move_rand.h"
#include "rcpp_move_behav.h"

using namespace Rcpp;

// [[Rcpp::interfaces(cpp)]]

//' rcpp_move_wrap
//'
//' @description
//' Rcpp movement behavior wrapper.
//'
//' @param fishpop Matrix with fishpop values.
//' @param pop_reserves_thres NumericVector with threshold of pop_reserves_max to drain prior to foraging.
//' @param movement String specifing movement algorithm. Either 'rand', 'attr' or 'behav'.
//' @param move_mean,move_var Double with mean movement parameter.
//' @param move_reef Double with mean movement distance when sheltering at reef.
//' @param move_border Double with movement distance that surrounds reef cell border.
//' @param move_return Double with mean movement distance when returning to reef.
//' @param max_dist Maximum distance an individual can move.
//' @param coords_reef Matrix with ID and coords of reef cells.
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
//' @keywords internal
// [[Rcpp::export(.rcpp_move_wrap)]]
void rcpp_move_wrap(Rcpp::NumericMatrix fishpop, Rcpp::NumericVector pop_reserves_thres,
                    Rcpp::String movement, double move_mean, double move_var, double move_reef,
                    double move_border, double move_return, double max_dist,
                    Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector extent,
                    Rcpp::IntegerVector dimensions) {

  // random movement
  if (movement == "rand") {

    rcpp_move_rand(fishpop, move_mean, move_var, max_dist, FALSE,
                   coords_reef, extent, dimensions);

    // attracted movement
  } else if (movement == "attr") {

    rcpp_move_rand(fishpop, move_mean, move_var, max_dist, TRUE,
                   coords_reef, extent, dimensions);

    // behaviour movement
  } else if (movement == "behav") {

    rcpp_move_behav(fishpop, pop_reserves_thres, move_mean, move_var,
                    move_reef, move_border, move_return, max_dist,
                    coords_reef, extent, dimensions);

    // throw error
  } else {

    Rcpp::stop("'movement' must be either 'rand', 'attr', or 'behav'.");

  }
}
