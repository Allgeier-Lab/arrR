#include "rcpp_mortality.h"
#include "rcpp_cell_from_xy.h"
#include "rcpp_shuffle.h"
#include "rcpp_reincarnate.h"

//' rcpp_mortality_backgr
//'
//' @description Rcpp background mortality
//'
//' @param fishpop,fishpop_track Matrix with fishpop and starting fishpop values.
//' @param seafloor Matrix with seafloor values.
//' @param pop_linf,pop_n_body,pop_want_reserves Numeric with parameters.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Rcpp implementation to create new individual after background mortality event.
//'
//' @return void
//'
//' @aliases rcpp_mortality_backgr
//' @rdname rcpp_mortality_backgr
//'
//' @export
// [[Rcpp::export]]
void rcpp_mortality(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                    Rcpp::NumericMatrix seafloor,
                    double pop_linf, double pop_n_body, double pop_want_reserves,
                    Rcpp::NumericVector extent, Rcpp::NumericVector dimensions) {

  // create random order if fish id because detritus can run out
  Rcpp::IntegerVector fish_id = rcpp_shuffle(0, fishpop.nrow() - 1);

  // loop through all fish ids
  for (int i = 0; i < fish_id.length(); i++) {

    // get current id of individual
    int fish_id_temp = fish_id(i);

    // get cell id of current individual
    int cell_id_temp = rcpp_cell_from_xy(NumericVector::create(fishpop(fish_id_temp, 2),
                                                               fishpop(fish_id_temp, 3)),
                                                               dimensions, extent) - 1;

    // create death probability
    double death_prob = std::exp(fishpop(fish_id_temp, 5) - pop_linf);

    // create random number to test death prob against
    double random_prob = Rcpp::runif(1, 0.0, 1.0)(0);

    // individual dies if random number is smaller than death probability
    if (random_prob < death_prob) {

      rcpp_reincarnate(fishpop, fishpop_track, seafloor,
                       fish_id_temp, cell_id_temp,
                       pop_linf, pop_n_body, pop_want_reserves,
                       "background");

    // skip current individual
    } else {

      continue;

    }
  }
}

/*** R
# create new individual
rcpp_mortality_backgr(fishpop = fishpop_values, fishpop_track = fishpop_track,
                      seafloor = seafloor_values,
                      pop_linf = parameters$pop_linf, pop_n_body = parameters$pop_n_body,
                      pop_want_reserves = parameters$pop_want_reserves,
                      extent = as.vector(extent, mode = "numeric"), dimensions = dimensions)
*/
