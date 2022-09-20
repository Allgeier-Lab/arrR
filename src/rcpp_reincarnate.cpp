#include <Rcpp.h>

#include "rcpp_reincarnate.h"
#include "rcpp_cell_from_xy.h"

using namespace Rcpp;

//' rcpp_reincarnate
//'
//' @description
//' Rcpp reincarnate fish indivudals.
//'
//' @param fishpop,fishpop_track Matrix with fishpop and starting fishpop values.
//' @param fish_id Vector with id of fish and corresponding cell ids.
//' @param seafloor Matrix with seafloor values.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//' @param pop_linf, pop_n_body, pop_reserves_max Numeric with parameters.
//' @param reason String with reason of reincarnation.
//'
//' @details
//' Creates a new individual after mortality event. The new individual has the same
//' values as the died individual at the beginning of the simulation.
//'
//' The mass difference (i.e. current mass minus mass at time step zero) and the
//' reserves of the died individual are added to the detritus pool. The reincarnated
//' individual tries to fill its reserves from the detritus pool if enough nutrients
//' are available.
//'
//' @return void
//'
//' @aliases rcpp_reincarnate
//' @rdname rcpp_reincarnate
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_reincarnate(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, int fish_id,
                      Rcpp::NumericMatrix seafloor, Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                      double pop_linf, double pop_n_body, double pop_reserves_max,
                      std::string reason) {

  int cell_id = 0;

  // kill old individual //

  // get cell id of old individual
  cell_id = rcpp_cell_from_xy(fishpop(fish_id, 2), fishpop(fish_id, 3),
                              extent, dimensions, true);

  // calculate increase in fish mass including reserves
  // mass_difference = weight - weight specific nutrient content + fish reserves
  double mass_diff = ((fishpop(fish_id, 6) - fishpop_track(fish_id, 6)) * pop_n_body) +
    fishpop(fish_id, 9);

  // add to dead detritus pool
  seafloor(cell_id, 6) += mass_diff;

  // save current mortality counter
  int died_consumption = fishpop(fish_id, 14);

  int died_background = fishpop(fish_id, 15);

  // save consumption and excretion
  double consumption = fishpop(fish_id, 12);

  double excretion = fishpop(fish_id, 13);

  // create new individual //

  // create new individual, access all columns of fish_id matrix
  fishpop(fish_id, _) = fishpop_track(fish_id, _);

  // set consumption and excretion to old values
  fishpop(fish_id, 12) = consumption;

  fishpop(fish_id, 13) = excretion;

  // set counters to old values
  fishpop(fish_id, 14) = died_consumption;

  fishpop(fish_id, 15) = died_background;

  // update mortality counter
  if (reason == "consumption") {

    fishpop(fish_id, 14) += 1;

  } else if (reason == "background") {

    fishpop(fish_id, 15) += 1;

  } else {

    Rcpp::stop("'reason' must be 'consumption' or 'background'.");

  }

  // fill reserves //

  // get cell id of new individual
  cell_id = rcpp_cell_from_xy(fishpop(fish_id, 2), fishpop(fish_id, 3),
                              extent, dimensions, true);

  // init consumed detritus
  double detritus_consumed = 0.0;

  // detritus pool is smaller than wanted reserves
  if (seafloor(cell_id, 5) <= fishpop(fish_id, 9)) {

    // fully consumes detritus pool in cell
    detritus_consumed = seafloor(cell_id, 5);

    // set pool to zero
    seafloor(cell_id, 5) = 0.0;

  // detritus pool is larger than wanted reserves
  } else {

    // consume wanted reserves
    detritus_consumed = fishpop(fish_id, 9);

    // reduce detritus pool by consumption
    seafloor(cell_id, 5) -= detritus_consumed;

  }

  fishpop(fish_id, 9) = detritus_consumed;

  // track consumption cell
  seafloor(cell_id, 13) += detritus_consumed;

  // track consumption fish
  fishpop(fish_id, 12) += detritus_consumed;

}
