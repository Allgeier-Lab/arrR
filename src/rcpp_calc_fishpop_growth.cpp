#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_calc_fishpop_growth
//'
//' @description Rcpp calc growth
//'
//' @param fishpop,fishpop_track Matrix with fishpop values and starting population.
//' @param seafloor Matrix with seafloor values.
//' @param fish_id,cell_id Vector with id of fish and corresponding cell ids.
//' @param pop_k_grunt,pop_linf_grunt,pop_a_grunt,pop_b_grunt Numeric with parameters.
//' @param pop_n_body,pop_max_reserves,pop_want_reserves,min_per_i Numeric with parameters.
//'
//' @details
//' Rcpp mplementation to calculate growth of fish individuals.
//'
//' @return void
//'
//' @aliases rcpp_calc_fishpop_growth
//' @rdname rcpp_calc_fishpop_growth
//'
//' @export
// [[Rcpp::export]]
void rcpp_calc_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                              Rcpp::NumericMatrix seafloor,
                              Rcpp::NumericVector fish_id, Rcpp::NumericVector cell_id,
                              double pop_k_grunt, double pop_linf_grunt,
                              double pop_a_grunt, double pop_b_grunt,
                              double pop_n_body, double pop_max_reserves, double pop_want_reserves,
                              double min_per_i) {

  // loop through all fish ids
  for (int i = 0; i < fish_id.length(); i++) {

    // create counter for temp fish id
    int fish_id_temp = fish_id(i) - 1;

    // create counter for temp cell id
    int cell_id_temp = cell_id(i) - 1;

     // # calculate growth in length and weight
    double growth_length = pop_k_grunt *
      (1.0 / 365.0) * (1.0 / 24.0) * (1.0 / 60.0) * min_per_i *
      (pop_linf_grunt - fishpop(fish_id_temp, 5));

    double growth_weight = pop_a_grunt *
      (std::pow((fishpop(fish_id_temp, 5) + growth_length), pop_b_grunt) -
      (std::pow(fishpop(fish_id_temp, 5), pop_b_grunt)));

    // calculate consumption requirements
    double consumption_req = ((growth_weight + fishpop(fish_id_temp, 10) *
                              fishpop(fish_id_temp, 6)) / 0.55) * pop_n_body;

    // individual dies because consumption requirements can not be met
    if (consumption_req > (seafloor(cell_id_temp, 5) + fishpop(fish_id_temp, 7))) {

      // save current original coordinates
      double x_coord = fishpop(fish_id_temp, 2);

      double y_coord = fishpop(fish_id_temp, 3);

      // save current mortality counter
      int died_consumption = fishpop(fish_id_temp, 11);

      int died_background = fishpop(fish_id_temp, 12);

      // calculate increase in fish mass including reserves
      double mass_diff = (fishpop(fish_id_temp, 6) - fishpop_track(fish_id_temp, 6)) * pop_n_body +
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
      fishpop(fish_id_temp, 11) = died_consumption + 1;

      fishpop(fish_id_temp, 12) = died_background;

    // individual grows because consumption requirements can be met
    } else {

      //  increase age (60 min * 24 h = 1440 min/day)
      fishpop(fish_id_temp, 1) += min_per_i / 1440.0;

      // increase fish dimensions length
      fishpop(fish_id_temp, 5) += growth_length;

      // increase fish dimensions weight
      fishpop(fish_id_temp, 6) += growth_weight;

      // update max reserves based on weight
      fishpop(fish_id_temp, 8) = fishpop(fish_id_temp, 6) * pop_n_body * pop_max_reserves;

      // calculate reserves difference
      double reserves_diff = fishpop(fish_id_temp, 8) - fishpop(fish_id_temp, 7);

      // consumption requirement can be meet by detritus_pool
      if (consumption_req <= seafloor(cell_id_temp, 5)) {

        // calculate remaining nutrients in pool
        double nutrients_left = seafloor(cell_id_temp, 5) - consumption_req;

        // reserves can be filled completely
        if (reserves_diff <= nutrients_left) {

          // save consumption
          seafloor(cell_id_temp, 8) += consumption_req + reserves_diff;

          // set reserves to max
          fishpop(fish_id_temp, 7) = fishpop(fish_id_temp, 8);

          // reduce detritus pool
          seafloor(cell_id_temp, 5) -= consumption_req + reserves_diff;

        // reserves cannot be filled completely by nutrient pool
        } else {

          // save consumption
          seafloor(cell_id_temp, 8) += consumption_req + nutrients_left;

          // add all nutrients that are left
          fishpop(fish_id_temp, 8) += nutrients_left;

          // set detritus pool to zero
          seafloor(cell_id_temp, 5) = 0;

        }

      // reserves are needed to meet consumption requirement
      } else {

        // save consumption
        seafloor(cell_id_temp, 8) += seafloor(cell_id_temp, 8);

        // reduced reserves
        fishpop(fish_id_temp, 7) -= consumption_req - seafloor(cell_id_temp, 5);

        // set detritus pool to zero
        seafloor(cell_id_temp, 5) = 0;

      }

      // calc non-used consumption (excretion)
      double excretion_temp = consumption_req - (growth_weight * pop_n_body);

      // save excretion
      seafloor(cell_id_temp, 9) += excretion_temp;

      // add non-used consumption to nutrient pool (excretion)
      seafloor(cell_id_temp, 4) += excretion_temp;

    }
  }
}

/*** R
rcpp_calc_fishpop_growth(fishpop = fishpop_values,
                         fishpop_track = fishpop_track,
                         seafloor = seafloor_values,
                         cell_id = cell_id,
                         pop_n_body = parameters$pop_n_body,
                         pop_max_reserves = parameters$pop_max_reserves,
                         pop_want_reserves = parameters$pop_want_reserves,
                         min_per_i = min_per_i)
*/
