#include <Rcpp.h>

#include "rcpp_mortality.h"

#include "rcpp_shuffle.h"
#include "rcpp_reincarnate.h"

using namespace Rcpp;

// [[Rcpp::interfaces(cpp)]]

// rcpp_mortality
//
// @description
// Rcpp simulate (background) mortality.
//
// @param fishpop,fishpop_track Matrix with fishpop and starting fishpop values.
// @param seafloor Matrix with seafloor values.
// @param pop_linf,pop_n_body,pop_reserves_max Numeric with parameters.
// @param extent Vector with extent (xmin,xmax,ymin,ymax).
// @param dimensions Vector with dimensions (nrow, ncol).
//
// @details
// Function to simulate background mortality of fish individuals. The mortality
// probability increases with increasing size and approximates p = 1 for \code{pop_linf}.
// If a individual dies, a new individual is created using \code{\link{rcpp_reincarnate}}.
//
// @return void
//
// @aliases rcpp_mortality
// @rdname rcpp_mortality
//
// @keywords internal
void rcpp_mortality(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                    Rcpp::NumericMatrix seafloor,
                    double pop_linf, double pop_n_body, double pop_reserves_max,
                    Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions) {

  // create random order if fish id because detritus can run out
  Rcpp::IntegerVector fish_id = rcpp_shuffle(0, fishpop.nrow() - 1);

  // loop through all fish ids
  for (int i = 0; i < fish_id.length(); i++) {

    // get current id of individual
    int fish_id_temp = fish_id[i];

    // create death probability
    double death_prob = std::exp(fishpop(fish_id_temp, 5) - pop_linf);

    // create random number to test death prob against
    double random_prob = Rcpp::runif(1, 0.0, 1.0)[0];

    // individual dies if random number is smaller than death probability
    if (random_prob < death_prob) {

      rcpp_reincarnate(fishpop, fishpop_track, fish_id_temp,
                       seafloor, extent, dimensions,
                       pop_linf, pop_n_body, pop_reserves_max,
                       "background");

    }
  }
}
