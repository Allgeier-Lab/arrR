#include "rcpp_reincarnate.h"

//' rcpp_reincarnate
//'
//' @description Rcpp reincarnate
//'
//' @param fishpop,fishpop_track Matrix with fishpop and starting fishpop values.
//' @param seafloor Matrix with seafloor values.
//' @param fish_id,cell_id Vector with id of fish and corresponding cell ids.
//' @param pop_linf,pop_n_body,pop_want_reserves Numeric with parameters.
//'
//' @details
//' Rcpp implementation to create new individual after mortality event.
//'
//' @return void
//'
//' @aliases rcpp_calc_mortality_consump
//' @rdname rcpp_calc_mortality_consump
//'
//' @export
// [[Rcpp::export]]
void rcpp_reincarnate(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                      Rcpp::NumericMatrix seafloor,
                      int fish_id, int cell_id,
                      double pop_linf, double pop_n_body, double pop_want_reserves,
                      String reason) {

  // save current original coordinates
  double x_coord = fishpop(fish_id, 2);

  double y_coord = fishpop(fish_id, 3);

  // save current mortality counter
  int died_consumption = fishpop(fish_id, 11);

  int died_background = fishpop(fish_id, 12);

  // calculate increase in fish mass including reserves
  // KSM: puts nutrients from dead fish into detrital pool
  // KSM: mass_difference = weight - weight specific nutrient content + fish reserves
  double mass_diff = (fishpop(fish_id, 6) -
                      fishpop_track(fish_id, 6)) * pop_n_body + fishpop(fish_id, 7);

  // add to dead detritus pool
  seafloor(cell_id, 6) += mass_diff;

  // create new individual
  // KSM: access all columns of fish_id DF
  fishpop(fish_id, _) = fishpop_track(fish_id, _);

  // keep old coordinates
  // KSM: put new fish back in place of dead fish
  fishpop(fish_id, 2) = x_coord;

  fishpop(fish_id, 3) = y_coord;

  // calculate wanted reserves
  // KSM: calculate new reserves for new fish of new size
  double reserves_wanted = pop_n_body * fishpop(fish_id, 6) * pop_want_reserves;

  // detritus pool is smaller than wanted reserves, detritus pool is fully used
  if (reserves_wanted >= seafloor(cell_id, 5)) {

    // use pool completely
    // KSM: fish fully consumes detritus pool in cell
    fishpop(fish_id, 7) = seafloor(cell_id, 5);

    // set pool to zero
    // KSM: cell goes to 0
    seafloor(cell_id, 5) = 0;

    // detritus pool is larger than what is wanted, so only subset is used
    // KSM: otherwise, if detritus pool is large enough, only a subset is used by fish
  } else {

    // wanted reserves can be filled completely
    fishpop(fish_id, 7) = reserves_wanted;

    // reduced detritus pool by wanted reserves
    // KSM: remove consumed detritus from cell to match reserve needs
    seafloor(cell_id, 5) -= reserves_wanted;

  }

  // update mortality counter
  if (reason == "consumption") {

    fishpop(fish_id, 11) = died_consumption + 1;

    fishpop(fish_id, 12) = died_background;

  } else if (reason == "background") {

    fishpop(fish_id, 11) = died_consumption;

    fishpop(fish_id, 12) = died_background + 1;

  } else {

  stop("'reason must be 'consumption' or 'background'");

  }

}

/*** R
rcpp_reincarnate(NumericMatrix fishpop,
                 Rcpp::NumericMatrix fishpop_track,
                 Rcpp::NumericMatrix seafloor,
                 int fish_id, int cell_id,
                 double pop_linf, double pop_n_body, double pop_want_reserves,
                 String reason)
*/
