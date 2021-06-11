#include "rcpp_fishpop_growth.h"
#include "rcpp_cell_from_xy.h"
#include "rcpp_reincarnate.h"
#include "rcpp_shuffle.h"

//' rcpp_fishpop_growth
//'
//' @description Rcpp fishpop growth
//'
//' @param fishpop,fishpop_track Matrix with fishpop values and starting population.
//' @param seafloor Matrix with seafloor values.
//' @param pop_k,pop_linf,pop_a,pop_b Numeric with parameters.
//' @param pop_n_body,pop_max_reserves,min_per_i Numeric with parameters.
//' @param pop_consumption_prop Double with consumption limit to fill reserves each timestep.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Function to simulate consumption, possible mortality, growth and excretion of fish
//' population.
//'
//' @references
//' Add reference
//'
//' @return void
//'
//' @aliases rcpp_fishpop_growth
//' @rdname rcpp_fishpop_growth
//'
//' @export
// [[Rcpp::export]]
void rcpp_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                         Rcpp::NumericMatrix seafloor,
                         double pop_k, double pop_linf, double pop_a, double pop_b,
                         double pop_n_body, double pop_max_reserves, double pop_consumption_prop,
                         Rcpp::NumericVector extent, Rcpp::NumericVector dimensions,
                         double min_per_i) {

  // create random order if fish id because detritus can run out
  Rcpp::IntegerVector fish_id = rcpp_shuffle(0, fishpop.nrow() - 1);

  // loop through all fish ids
  for (int i = 0; i < fish_id.length(); i++) {

    // create counter for temp fish id because randomized in simulate_fishpop_growth
    int fish_id_temp = fish_id(i);

    // get cell id of current individual
    int cell_id_temp = rcpp_cell_from_xy(NumericVector::create(fishpop(fish_id_temp, 2),
                                                               fishpop(fish_id_temp, 3)),
                                                               dimensions, extent) - 1;

    // calculate growth in length and weight
    double growth_length = pop_k *
      (1.0 / 365.0) * (1.0 / 24.0) * (1.0 / 60.0) * min_per_i *
      (pop_linf - fishpop(fish_id_temp, 5));

    // length intial + change in length^b
    double growth_weight = pop_a *
      (std::pow((fishpop(fish_id_temp, 5) + growth_length), pop_b) -
      (std::pow(fishpop(fish_id_temp, 5), pop_b)));

    // consumption req based on growth in weight + metabolic costs based on weight + n required
    double consumption_require = ((growth_weight + fishpop(fish_id_temp, 8) *
                                  fishpop(fish_id_temp, 6)) / 0.55) * pop_n_body;

    // check mortality behavior 3 (foraging, reserves + detritus available)
    if (fishpop(fish_id_temp, 11) == 3.0) {

      // individual dies because consumption requirements cannot be met by
      // detritus in cell and reserves
      if (consumption_require > (seafloor(cell_id_temp, 5) + fishpop(fish_id_temp, 9))) {

        rcpp_reincarnate(fishpop, fishpop_track, seafloor,
                         fish_id_temp, cell_id_temp,
                         pop_linf, pop_n_body, pop_max_reserves,
                         "consumption");

        continue;

      // individual grows because consumption requirements can be met
      } else {

        //  increase age (60 min * 24 h = 1440 min/day)
        fishpop(fish_id_temp, 1) += (min_per_i / 1440.0);

        // increase fish dimensions length
        fishpop(fish_id_temp, 5) += growth_length;

        // increase fish dimensions weight
        fishpop(fish_id_temp, 6) += growth_weight;

        // consumption requirement can be met by detritus_pool
        if (consumption_require <= seafloor(cell_id_temp, 5)) {

          // init consumption to fill up reserves
          double consumption_reserve = 0.0;

          // remove consumption for growth amount from cell
          seafloor(cell_id_temp, 5) -= consumption_require;

          // calculate amount that fish can eat per cell to fill reserves based on
          // maximum reserves
          double consumption_limit = pop_consumption_prop * fishpop(fish_id_temp, 10);

          // calculate difference between reserves max and current reserves
          double nutrients_diff = fishpop(fish_id_temp, 10) - fishpop(fish_id_temp, 9);

          // reserves can be filled completely
          if (consumption_limit >= nutrients_diff
                && seafloor(cell_id_temp, 5) >= nutrients_diff) {

            consumption_reserve = nutrients_diff;

          // reserves cannot be filled completely i) limits or ii) pool
          } else {

            // limits are smaller than detritus pool
            if (consumption_limit <= seafloor(cell_id_temp, 5)) {

              consumption_reserve = consumption_limit;

            // detritus pool is smaller than limits
            } else {

              consumption_reserve = seafloor(cell_id_temp, 5);

            }
          }

          // reduce detritus pool by reserves
          seafloor(cell_id_temp, 5) -= consumption_reserve;

          // increase reserves
          fishpop(fish_id_temp, 9) += consumption_reserve;

          // track consumption cell
          seafloor(cell_id_temp, 13) += (consumption_require + consumption_reserve);

          // track consumption fish
          fishpop(fish_id_temp, 12) += (consumption_require + consumption_reserve);

        // reserves are needed to meet consumption requirement
        } else {

          // reduced reserves because there was not enough detritus in cell
          fishpop(fish_id_temp, 9) -= (consumption_require - seafloor(cell_id_temp, 5));

          // set detritus pool to zero
          seafloor(cell_id_temp, 5) = 0.0;

          // track consumption cell
          seafloor(cell_id_temp, 13) += seafloor(cell_id_temp, 5);

          // track consumption fish
          fishpop(fish_id_temp, 12) += seafloor(cell_id_temp, 5);

        }
      }

    // check mortality behavior 1 (@reef) and 2 (returning) (only reserves avail)
    } else {

      // MH: This would be where Issue #53 comes into play
      // individual dies because consumption requirements cannot be met by reserves
      if (consumption_require > fishpop(fish_id_temp, 9)) {

        rcpp_reincarnate(fishpop, fishpop_track, seafloor,
                         fish_id_temp, cell_id_temp,
                         pop_linf, pop_n_body, pop_max_reserves,
                         "consumption");

        continue;

      // individual grows because consumption requirements can be met
      } else {

        // increase age (60 min * 24 h = 1440 min/day)
        fishpop(fish_id_temp, 1) += (min_per_i / 1440.0);

        // fish uses reserves to meet consumption requirements
        fishpop(fish_id_temp, 9) -= consumption_require;

        // increase fish dimensions length
        fishpop(fish_id_temp, 5) += growth_length;

        // increase fish dimensions weight
        fishpop(fish_id_temp, 6) += growth_weight;

      }
    }

    // calc non-used consumption (excretion)
    double excretion_temp = (consumption_require - (growth_weight * pop_n_body));

    // add non-used consumption to nutrient pool
    seafloor(cell_id_temp, 4) += excretion_temp;

    // track excretion cell
    seafloor(cell_id_temp, 14) += excretion_temp;

    // track excretion fish
    fishpop(fish_id_temp, 13) += excretion_temp;

    // update max reserves based on weight
    fishpop(fish_id_temp, 10) = fishpop(fish_id_temp, 6) * pop_n_body * pop_max_reserves;

  }
}

/*** R
rcpp_fishpop_growth(fishpop = fishpop_values,
                    fishpop_track = fishpop_track[[1]], seafloor = seafloor_values,
                    pop_k = parameters$pop_k, pop_linf = parameters$pop_linf,
                    pop_a = parameters$pop_a, pop_b = parameters$pop_b,
                    pop_n_body = parameters$pop_n_body,
                    pop_max_reserves = parameters$pop_max_reserves,
                    min_per_i = min_per_i
*/
