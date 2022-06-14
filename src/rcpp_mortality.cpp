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
//' @param pop_linf,pop_n_body,pop_reserves_max Vector with parameters.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Function to simulate background mortality of fish individuals. The mortality
//' probability increases with increasing size and approximates p=1 for \code{pop_linf}.
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
                    Rcpp::NumericVector pop_linf,  Rcpp::NumericVector pop_n_body,
                    Rcpp::NumericVector pop_reserves_max,
                    Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions) {

  // create random order if fish id because detritus can run out
  Rcpp::NumericVector row_id = rcpp_shuffle(fishpop(_, 0), false);

  // loop through all fish ids
  for (int i = 0; i < row_id.length(); i++) {

    // use Rcpp indexing counter of current loop iteration
    int row_id_temp = row_id[i] - 1;

    // get current species id
    int species_temp = fishpop(row_id_temp, 1) - 1;

    // create death probability
    double death_prob = std::exp(fishpop(row_id_temp, 6) - pop_linf[species_temp]);

    // create random number to test death prob against
    double random_prob = rcpp_runif(0.0, 1.0);

    // individual dies if random number is smaller than death probability
    if (random_prob < death_prob) {

      rcpp_reincarnate(fishpop, fishpop_track, row_id_temp,
                       seafloor, extent, dimensions,
                       pop_linf[species_temp], pop_n_body[species_temp], pop_reserves_max[species_temp],
                       "background");

    }
  }
}
