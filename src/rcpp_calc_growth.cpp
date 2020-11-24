#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_calc_growth
//'
//' @description Rcpp calc growth
//'
//' @param fishpop Matrix with fishpop values.
//' @param seafloor Matrix with seafloor values.
//' @param growth_values Matrix with growth values.
//' @param pop_n_body,pop_max_reserves,min_per_i Numeric with parameters.
//'
//' @details
//' \code{Rcpp} implementation of to calculate growth.
//'
//' @return Matrix
//'
//' @name rcpp_calc_growth
//'
//' @export
// [[Rcpp::export]]
void rcpp_calc_growth(Rcpp::NumericMatrix fishpop,
                      Rcpp::NumericMatrix fishpop_track,
                      Rcpp::NumericMatrix seafloor,
                      Rcpp::NumericVector cell_id,
                      Rcpp::NumericMatrix growth_values,
                      double pop_n_body,
                      double pop_max_reserves,
                      double pop_want_reserves,
                      double min_per_i) {

  for (int i = 0; i < fishpop.nrow(); i++) {

    // create counter for temp cell id
    int cell_id_temp = cell_id(i);

    // individual dies because consumption requirements can not be met
    if (growth_values(i, 0) > (seafloor(cell_id_temp, 5) + fishpop(i, 7))) {

      // extract original coordinates
      double x_coord = fishpop(i, 2);
      double y_coord = fishpop(i, 3);

      // extract mortality counter
      int died_consumption = fishpop(i, 11);

      int died_background = fishpop(i, 12);

      // calculate increase in fish mass including reserves
      double mass_diff = ((fishpop(i, 6) - fishpop_track(i, 6)) * pop_n_body) +
        fishpop(i, 7);

      // add to dead detritus pool
      seafloor(cell_id_temp, 6) += mass_diff;

      // create new individual
      fishpop(i, _) = fishpop_track(i, _);

      // keep old coordinates
      fishpop(i, 2) = x_coord;

      fishpop(i, 3) = y_coord;

      // calculate wanted reserves
      double reserves_wanted = pop_n_body * fishpop(i, 6) * pop_want_reserves;

      // detritus pool is smaller than wanted reserves, detritus pool is fully used
      if (reserves_wanted >= seafloor(cell_id_temp, 5)) {

        fishpop(i, 7) = seafloor(cell_id_temp, 5);

        seafloor(cell_id_temp, 5) = 0;

      // detritus pool is larger than what is wanted, so only subset is used
      } else {

        fishpop(i, 7) = reserves_wanted;

        seafloor(cell_id_temp, 5) -= reserves_wanted;

      }

      // update mortality counter
      fishpop(i, 11) = died_consumption + 1;

      fishpop(i, 12) = died_background;

    // individual grows because consumption requirements can be met
    } else {

      //  increase age (60 min * 24 h = 1440 min/day)
      fishpop(i, 1) += min_per_i / 1440;

      // increase fish dimensions length
      fishpop(i, 5) += growth_values(i, 1);

      // increase fish dimensions weight
      fishpop(i, 6) += growth_values(i, 2);

      // update max reserves based on weight
      fishpop(i, 8) = fishpop(i, 6) * pop_n_body * pop_max_reserves;

      // calculate reserves difference
      double reserves_diff = fishpop(i, 8) - fishpop(i, 7);

      // consumption requirement can be meet by detritus_pool
      if (growth_values(i, 0) <= seafloor(cell_id_temp, 5)) {

        // calculate remaining nutrients in pool
        double nutrients_left = seafloor(cell_id_temp, 5) - growth_values(i, 0);

        // reserves can be filled completely
        if (reserves_diff <= nutrients_left) {

          // save consumption
          seafloor(cell_id_temp, 8) += growth_values(i, 0) + reserves_diff;

          // set reserves to max
          fishpop(i, 7) = fishpop(i, 8);

          // reduce detritus pool
          seafloor(cell_id_temp, 5) = nutrients_left - reserves_diff;

        // reserves cannot be filled completely by nutrient pool
        } else {

          // save consumption
          seafloor(cell_id_temp, 8) += growth_values(i, 0) + nutrients_left;

          // add all nutrients that are left
          fishpop(i, 8) += nutrients_left;

          // set detritus pool to zero
          seafloor(cell_id_temp, 5) = 0;
        }

      // reserves are needed to meet consumption requirement
      } else {

        // save consumption
        seafloor(cell_id_temp, 8) += seafloor(cell_id_temp, 8);

        // reduced reserves
        fishpop(i, 7) -= growth_values(i, 0) - seafloor(cell_id_temp, 5);

        // set detritus pool to zero
        seafloor(cell_id_temp, 5) = 0;

      }

      // calc non-used consumption (excretion)
      double excretion_temp = growth_values(i, 0) - (growth_values(i, 2) * pop_n_body);

      // save excretion
      seafloor(cell_id_temp, 9) += excretion_temp;

      // add non-used consumption to nutrient pool (excretion)
      seafloor(cell_id_temp, 4) += excretion_temp;

    }
  }
}

/*** R
rcpp_calc_growth(fishpop = fishpop_values,
                 fishpop_track = fishpop_track[[1]],
                 seafloor = seafloor_values,
                 cell_id = cell_id,
                 growth_values = growth_values,
                 pop_n_body = parameters$pop_n_body,
                 pop_max_reserves = parameters$pop_max_reserves,
                 pop_want_reserves = parameters$pop_want_reserves,
                 min_per_i = min_per_i)
*/
