#include "rcpp_reincarnate.h"

//' rcpp_calc_fishpop_growth
//'
//' @description Rcpp calc growth
//'
//' @param fishpop,fishpop_track Matrix with fishpop values and starting population.
//' @param seafloor Matrix with seafloor values.
//' @param fish_id,cell_id Vector with id of fish and corresponding cell ids.
//' @param pop_k,pop_linf,pop_a,pop_b Numeric with parameters.
//' @param pop_n_body,pop_max_reserves,pop_want_reserves,min_per_i Numeric with parameters.
//' @param pop_thres_reserves Vector with threshold of pop_max_reserves to drain prior to foraging
//'
//' @details
//' Rcpp implementation to calculate growth of fish individuals.
//' "KSM" notes from Katrina to help understand code
//' "Q": questions for Max
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
                              Rcpp::NumericVector pop_thres_reserves,
                              double pop_k, double pop_linf,
                              double pop_a, double pop_b,
                              double pop_n_body, double pop_max_reserves, double pop_want_reserves,
                              double min_per_i) {

  // loop through all fish ids
  for (int i = 0; i < fish_id.length(); i++) {

    // create counter for temp fish id
    // KSM: sets up a temporary order of fish ID,
    // so Fish 1 does not get all of the resources in each patch
    int fish_id_temp = fish_id(i) - 1;

    // create counter for temp cell id
    int cell_id_temp = cell_id(i) - 1;

    // calculate growth in length and weight
    // KSM: calculate growth per minute
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

    Rcout << "initial length:" << fishpop(fish_id_temp, 5) << std::endl;

    // check mortality Behavior 3

    if (fishpop(fish_id_temp, 14) == 3.0) {

      // calculate amount of available resources
      // KSM: available resources = resources (detritus pool) per cell + fish reserves (per cell)

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

        // calculate reserves difference
        // KSM: difference in reserves = reserves_max - current reserves
        double reserves_diff = fishpop(fish_id_temp, 8) - fishpop(fish_id_temp, 7);

        // consumption requirement can be met by detritus_pool
        if (consumption_req <= seafloor(cell_id_temp, 5)) {

          // calculate remaining nutrients in pool
          double nutrients_left = seafloor(cell_id_temp, 5) - consumption_req;

          // reserves can be filled completely in cell
          if (reserves_diff <= nutrients_left) {

            // set reserves to max
            fishpop(fish_id_temp, 7) = fishpop(fish_id_temp, 8);

            // reduce detritus pool
            seafloor(cell_id_temp, 5) -= (consumption_req + reserves_diff);

            // track consumption
            seafloor(cell_id_temp, 13) += consumption_req + reserves_diff;

            Rcout << "consumption L117:" << seafloor(cell_id_temp, 13) << std::endl;

            // reserves cannot be filled completely by nutrient pool
          } else {

            // add all nutrients that are left
            fishpop(fish_id_temp, 7) += nutrients_left;

            // set detritus pool to zero
            seafloor(cell_id_temp, 5) = 0;

            // track consumption
            seafloor(cell_id_temp, 13) += consumption_req + nutrients_left;

          }

          // reserves are needed to meet consumption requirement
        } else {

          // reduced reserves
          // KSM: because there was not enough detritus in cell, reserves are reduced (after consuming detritus that was available)
          fishpop(fish_id_temp, 7) -= (consumption_req - seafloor(cell_id_temp, 5));

          // track consumption
          // KSM: all of that detritus pool is added to consumption in each cell
          seafloor(cell_id_temp, 13) += seafloor(cell_id_temp, 5);

          // set detritus pool to zero
          seafloor(cell_id_temp, 5) = 0;

        }

      }

      // check mortality Behavior 1 and 2
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

        //  increase age (60 min * 24 h = 1440 min/day)
        fishpop(fish_id_temp, 1) += (min_per_i / 1440.0);

        // increase fish dimensions length
        fishpop(fish_id_temp, 5) += growth_length;

        // increase fish dimensions weight
        fishpop(fish_id_temp, 6) += growth_weight;

        // update max reserves based on weight
        fishpop(fish_id_temp, 8) = fishpop(fish_id_temp, 6) * pop_n_body * pop_max_reserves;

        // fish uses reserves to meet consumption requirements
        // KSM: reduce reserves to meet consumption_req
        fishpop(fish_id_temp, 7) -= consumption_req;

        // safety check
        if (fishpop(fish_id_temp, 7) > 0) {

          stop("consumption is non-zero...which it shouldn't!");

        }

      }

        // calc non-used consumption (excretion)
        double excretion_temp = consumption_req - (growth_weight * pop_n_body);

        // track excretion
        seafloor(cell_id_temp, 14) += excretion_temp;

        // add non-used consumption to nutrient pool
        seafloor(cell_id_temp, 4) += excretion_temp;

        Rcout << "final length:" << fishpop(fish_id_temp, 5) << std::endl;

      }
    }
  }

/*** R
rcpp_calc_fishpop_growth(fishpop = fishpop_values,
                         fishpop_track = fishpop_track,
                         seafloor = seafloor_values,
                         fish_id = fish_id, cell_id = cell_id,
                         pop_k = parameters$pop_k,
                         pop_linf = parameters$pop_linf,
                         pop_a = parameters$pop_a,
                         pop_b = parameters$pop_b,
                         pop_n_body = parameters$pop_n_body,
                         pop_max_reserves = parameters$pop_max_reserves,
                         pop_want_reserves = parameters$pop_want_reserves,
                         pop_thres_reserves = pop_thres_reserves,
                         min_per_i = min_per_i,
                         behavior = behavior)
*/
