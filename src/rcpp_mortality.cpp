#include <Rcpp.h>

#include "rcpp_mortality.h"
#include "rcpp_shuffle.h"
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
//' @param pop_ldie, pop_n_body,pop_reserves_max Numeric with parameters.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Function to simulate background mortality of fish individuals.
//' Fish automatically dies when \code{pop_mean_size} + 2.
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
                    Rcpp::NumericMatrix seafloor,
                    double pop_ldie, double pop_n_body, double pop_reserves_max,
                    Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions) {

  // create random order if fish id because detritus can run out
  Rcpp::NumericVector row_id = rcpp_shuffle(fishpop(_, 0), false);

  // loop through all fish ids
  for (int i = 0; i < row_id.length(); i++) {

    // use Rcpp indexing counter of current loop iteration
    int row_id_temp = row_id[i] - 1;

    // individual dies if current size is larger than pop_mean_size + X
    if (fishpop(row_id_temp, 5) > pop_ldie) {

      rcpp_reincarnate(fishpop, fishpop_track, row_id_temp,
                       seafloor, extent, dimensions,
                       pop_ldie, pop_n_body, pop_reserves_max,
                       "background");

    }
  }
}
