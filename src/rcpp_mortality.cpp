#include <Rcpp.h>

#include "rcpp_mortality.h"
#include "rcpp_shuffle.h"
#include "rcpp_runif.h"
#include "rcpp_reincarnate.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_mortality
//'
//' @description
//' Rcpp simulate (background) mortality.
//'
//' @param fishpop,fishpop_track Matrix with fishpop and starting fishpop values.
//' @param seafloor Matrix with seafloor values.
//' @param pop_linf,pop_ldie,pop_n_body,pop_reserves_max Numeric with parameters.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Function to simulate background mortality of fish individuals.
//' Fish automatically dies when current size is greater than \code{pop_ldie}.
//' If a individual dies, a new individual is created.
//'
//' @return void
//'
//' @aliases rcpp_mortality
//' @rdname rcpp_mortality
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_mortality(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                    Rcpp::NumericMatrix seafloor, double pop_linf, double pop_ldie, double pop_n_body,
                    double pop_reserves_max, Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions) {

  // create random order if fish id because detritus can run out
  Rcpp::NumericVector row_id = rcpp_shuffle(fishpop(_, 0), false);

  // loop through all fish ids
  for (int i = 0; i < row_id.length(); i++) {

    // init flag for mortality
    bool flag_die = false;

    // use Rcpp indexing counter of current loop iteration
    int row_id_temp = row_id[i] - 1;

    // use fixed mortality
    if (pop_ldie > 0.0) {

      // update mortality flag based on current size and ldie
      flag_die = fishpop(row_id_temp, 5) > pop_ldie;

    // use probability based mortality
    } else {

      // create death probability
      double death_prob = std::exp(fishpop(row_id_temp, 5) - pop_linf);

      // create random number to test death prob against
      double random_prob = rcpp_runif(0.0, 1.0);

      // update mortality flag based on probability
      flag_die = random_prob < death_prob;

    }

    // individual dies if flag is TRUE
    if (flag_die) {

      rcpp_reincarnate(fishpop, fishpop_track, row_id_temp,
                       seafloor, extent, dimensions, pop_n_body, pop_reserves_max,
                       "background");

    }
  }
}
