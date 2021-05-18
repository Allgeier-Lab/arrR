#include "rcpp_fishpop_growth.h"
#include "rcpp_cell_from_xy.h"
#include "rcpp_reincarnate.h"

//' rcpp_fishpop_growth
//'
//' @description Rcpp calc growth
//'
//' @param fishpop,fishpop_track Matrix with fishpop values and starting population.
//' @param fish_id Vector with id of fish and corresponding cell ids.
//' @param seafloor Matrix with seafloor values.
//' @param pop_k,pop_linf,pop_a,pop_b Numeric with parameters.
//' @param pop_n_body,pop_want_reserves,pop_max_reserves,min_per_i Numeric with parameters.
//' @param pop_consumption_prop Double with consumption limit to fill reserves each timestep.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//' @param min_per_i Integer to specify minutes per i.
//'
//' @details
//' Rcpp implementation to calculate growth of fish individuals.
//'
//' @return void
//'
//' @aliases rcpp_fishpop_growth
//' @rdname rcpp_fishpop_growth
//'
//' @export
// [[Rcpp::export]]
void rcpp_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericVector fish_id,
                         Rcpp::NumericMatrix fishpop_track, Rcpp::NumericMatrix seafloor,
                         double pop_k, double pop_linf, double pop_a, double pop_b,
                         double pop_n_body, double pop_want_reserves, double pop_max_reserves,
                         double pop_consumption_prop,
                         Rcpp::NumericVector extent, Rcpp::NumericVector dimensions,
                         double min_per_i) {

  // loop through all fish ids
  for (int i = 0; i < fish_id.length(); i++) {

    // create counter for temp fish id because randomized in simulate_fishpop_growth
    int fish_id_temp = fish_id(i) - 1;

    // get cell id of current individual
    int cell_id_temp = rcpp_cell_from_xy(NumericVector::create(fishpop(fish_id_temp, 2),
                                                               fishpop(fish_id_temp, 3)),
                                                               dimensions, extent) - 1;

    // calculate growth in length and weight
    double growth_length = pop_k *
      (1.0 / 365.0) * (1.0 / 24.0) * (1.0 / 60.0) * min_per_i *
      (pop_linf - fishpop(fish_id_temp, 5));

    // KSM: length intial + change in length^b
    double growth_weight = pop_a *
      (std::pow((fishpop(fish_id_temp, 5) + growth_length), pop_b) -
      (std::pow(fishpop(fish_id_temp, 5), pop_b)));

    // calculate consumption requirements
    // KSM: consumption req based on growth in weight + metabolic costs based on weight + n required
    double consumption_req = ((growth_weight + fishpop(fish_id_temp, 10) *
                              fishpop(fish_id_temp, 6)) / 0.55) * pop_n_body;

    // check mortality behavior 3 (foraging, reserves + detritus available)
    if (fishpop(fish_id_temp, 13) == 3.0) {

      // calculate amount of available resources (detritus + reserves)
      double available_resources = seafloor(cell_id_temp, 5) + fishpop(fish_id_temp, 7);

      // individual dies because consumption requirements cannot be met
      if (consumption_req > available_resources) {

        rcpp_reincarnate(fishpop, fishpop_track, seafloor,
                         fish_id_temp, cell_id_temp,
                         pop_linf, pop_n_body, pop_want_reserves,
                         "consumption");

      // individual grows because consumption requirements can be met
      } else {

        //  increase age (60 min * 24 h = 1440 min/day)
        fishpop(fish_id_temp, 1) += (min_per_i / 1440.0);

        // increase fish dimensions length
        fishpop(fish_id_temp, 5) += growth_length;

        // increase fish dimensions weight
        fishpop(fish_id_temp, 6) += growth_weight;

        // update max reserves based on weight
        fishpop(fish_id_temp, 8) = fishpop(fish_id_temp, 6) * pop_n_body * pop_max_reserves;

        // consumption requirement can be met by detritus_pool
        if (consumption_req <= seafloor(cell_id_temp, 5)) {

          // calculate remaining nutrients in pool
          double nutrients_left = seafloor(cell_id_temp, 5) - consumption_req;

          // calculate amount that fish can eat per cell to fill reserves
          double consumption_limit = pop_consumption_prop * fishpop(fish_id_temp, 8);

          // reserves (limit based on consumption_prop) can be filled in cell
          if (nutrients_left >= consumption_limit) {

            // increase reserves based on consumption_prop
            fishpop(fish_id_temp, 7) += consumption_limit;

            // reduce detritus pool
            seafloor(cell_id_temp, 5) -= (consumption_req + consumption_limit);

            // track consumption
            seafloor(cell_id_temp, 13) += (consumption_req + consumption_limit);

          // reserves (limit based on consumption_prop) cannot be filled completely by nutrient pool
          } else {

            // add all nutrients that are left
            fishpop(fish_id_temp, 7) += nutrients_left;

            // set detritus pool to zero
            seafloor(cell_id_temp, 5) = 0;

            // track consumption
            seafloor(cell_id_temp, 13) += (consumption_req + nutrients_left);

          }

        // reserves are needed to meet consumption requirement
        } else {

          // reduced reserves because there was not enough detritus in cell,
          fishpop(fish_id_temp, 7) -= (consumption_req - seafloor(cell_id_temp, 5));

          // track consumption
          seafloor(cell_id_temp, 13) += seafloor(cell_id_temp, 5);

          // set detritus pool to zero
          seafloor(cell_id_temp, 5) = 0;

        }
      }

    // check mortality behavior 1 (@reef) and 2 (returning) (only reserves avail)
    } else {

      // MH: This would be where Issue #53 comes into play
      // individual dies because consumption requirements cannot be met by reserves
      if (consumption_req > fishpop(fish_id_temp, 7)) {

        rcpp_reincarnate(fishpop, fishpop_track, seafloor,
                         fish_id_temp, cell_id_temp,
                         pop_linf, pop_n_body, pop_want_reserves,
                         "consumption");

      // individual grows because consumption requirements can be met
      } else {

        // increase age (60 min * 24 h = 1440 min/day)
        fishpop(fish_id_temp, 1) += (min_per_i / 1440.0);

        // increase fish dimensions length
        fishpop(fish_id_temp, 5) += growth_length;

        // increase fish dimensions weight
        fishpop(fish_id_temp, 6) += growth_weight;

        // update max reserves based on weight
        fishpop(fish_id_temp, 8) = fishpop(fish_id_temp, 6) * pop_n_body * pop_max_reserves;

        // fish uses reserves to meet consumption requirements
        fishpop(fish_id_temp, 7) -= consumption_req;

      }
    }

    // calc non-used consumption (excretion)
    double excretion_temp = consumption_req - (growth_weight * pop_n_body);

    // track excretion
    seafloor(cell_id_temp, 14) += excretion_temp;

    // add non-used consumption to nutrient pool
    seafloor(cell_id_temp, 4) += excretion_temp;

  }
}

/*** R
rcpp_fishpop_growth(fishpop = fishpop_values, fish_id = fish_id, cell_id = cell_id,
                    fishpop_track = fishpop_track, seafloor = seafloor_values,
                    pop_k = parameters$pop_k, pop_linf = parameters$pop_linf,
                    pop_a = parameters$pop_a, pop_b = parameters$pop_b,
                    pop_n_body = parameters$pop_n_body,
                    pop_want_reserves = parameters$pop_want_reserves,
                    pop_max_reserves = parameters$pop_max_reserves,
                    min_per_i = min_per_i
*/
