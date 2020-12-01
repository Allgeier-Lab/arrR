#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_create_rebirth
//'
//' @description Rcpp create new individual
//'
//' @param fishpop,fishpop_track Matrix with fishpop values.
//' @param seafloor Matrix with seafloor values.
//' @param fish_id,cell_id Vector with id of fish and corresponding cell ids.
//' @param pop_n_body,pop_want_reserves Numeric with parameters.
//'
//' @details
//' \code{Rcpp} implementation of to calculate growth.
//'
//' @return Matrix
//'
//' @aliases rcpp_create_rebirth
//' @rdname rcpp_create_rebirth
//'
//' @export
// [[Rcpp::export]]
void rcpp_create_rebirth(Rcpp::NumericMatrix fishpop,
                         Rcpp::NumericMatrix fishpop_track,
                         Rcpp::NumericMatrix seafloor,
                         Rcpp::NumericVector fish_id,
                         Rcpp::NumericVector cell_id,
                         double pop_n_body,
                         double pop_want_reserves) {

  for (int i = 0; i < fish_id.length(); i++) {

    // create counter for fish track id
    int fish_id_temp = fish_id(i) - 1;

    // create counter for temp cell id
    int cell_id_temp = cell_id(i);

    // save current original coordinates
    double x_coord = fishpop(fish_id_temp, 2);

    double y_coord = fishpop(fish_id_temp, 3);

    // save current mortality counter
    int died_consumption = fishpop(fish_id_temp, 11);

    int died_background = fishpop(fish_id_temp, 12);

    // calculate increase in fish mass including reserves
    double mass_diff = ((fishpop(fish_id_temp, 6) - fishpop_track(fish_id_temp, 6)) * pop_n_body) +
      fishpop(fish_id_temp, 7);

    // add to dead detritus pool
    seafloor(cell_id_temp, 6) += mass_diff;

    // create new individual
    fishpop(fish_id_temp, _) = fishpop_track(fish_id_temp, _);

    // keep old coordinates
    fishpop(fish_id_temp, 2) = x_coord;

    fishpop(fish_id_temp, 3) = y_coord;

    // calculate wanted reserves
    double reserves_wanted = pop_n_body * fishpop(fish_id_temp, 6) * pop_want_reserves;

    // detritus pool is smaller than wanted reserves, detritus pool is fully used
    if (reserves_wanted >= seafloor(cell_id_temp, 5)) {

      fishpop(fish_id_temp, 7) = seafloor(cell_id_temp, 5);

      seafloor(cell_id_temp, 5) = 0;

    // detritus pool is larger than what is wanted, so only subset is used
    } else {

      fishpop(fish_id_temp, 7) = reserves_wanted;

      seafloor(cell_id_temp, 5) -= reserves_wanted;

    }

    // update mortality counter
    fishpop(fish_id_temp, 11) = died_consumption;

    fishpop(fish_id_temp, 12) = died_background + 1;

  }
}