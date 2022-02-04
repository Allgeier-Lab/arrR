#include <Rcpp.h>

#include "rcpp_fishpop_growth.h"

#include "rcpp_cell_from_xy.h"
#include "rcpp_reincarnate.h"
#include "rcpp_shuffle.h"

using namespace Rcpp;

// [[Rcpp::interfaces(cpp)]]

// rcpp_fishpop_growth
//
// @description
// Rcpp simulate fishpop growth.
//
// @param fishpop,fishpop_track Matrix with fishpop values and starting population.
// @param seafloor Matrix with seafloor values.
// @param pop_k,pop_linf,pop_a,pop_b Numeric with parameters.
// @param pop_n_body,pop_reserves_max,min_per_i Numeric with parameters.
// @param pop_reserves_consump Double with consumption limit to fill reserves each timestep.
// @param extent Vector with extent (xmin,xmax,ymin,ymax).
// @param dimensions Vector with dimensions (nrow, ncol).
//
// @details
// Function to simulate consumption, mortality, growth and excretion of fish
// individuals. First each fish individual has to follow the von Bertalanffy growth curve
// and the corresponding amount of nutrients for this are calculated based on a
// bioenergetics model (Allgeier et al. 2020). The nutrients are consumed from the
// detritus_pool in the cell each individual is located in. If the available amount is not big
// enough, fish can either use their reserves or they die (see \code{\link{rcpp_reincarnate}}).
// Last, if the detritus pool is big enough, individuals can additionally fill up
// their reserves.
//
// If individuals are within behavior 1 or 2 (only for \code{movement = behav}),
// the consumption requirement must be met by the reserves only.
//
// If \code{0 > pop_reserves_consump < 1}, only a ratio of the \code{pop_reserves_max}
// can be consumed each timestep.
//
// @references
// Allgeier, J.E., Cline, T.J., Walsworth, T.E., Wathen, G., Layman, C.A.,
// Schindler, D.E., 2020. Individual behavior drives ecosystem function and the impacts of
// harvest. Sci. Adv. 6, eaax8329. <https://doi.org/10.1126/sciadv.aax8329>
//
// Froese, R., Pauly, D., 2019. FishBase. World Wide Web electronic publication
// [WWW Document]. <www.fishbase.org>
//
// @return void
//
// @aliases rcpp_fishpop_growth
// @rdname rcpp_fishpop_growth
//
// @keywords internal
void rcpp_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                         Rcpp::NumericMatrix seafloor,
                         double pop_k, double pop_linf, double pop_a, double pop_b,
                         double pop_n_body, double pop_reserves_max, double pop_reserves_consump,
                         Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                         double min_per_i) {

  // create random order if fish id because detritus can run out
  Rcpp::IntegerVector fish_id = rcpp_shuffle(0, fishpop.nrow() - 1);

  // loop through all fish ids
  for (int i = 0; i < fish_id.length(); i++) {

    // create counter for temp fish id because randomized in simulate_fishpop_growth
    int fish_id_temp = fish_id[i];

    // get cell id of current individual
    int cell_id_temp = rcpp_cell_from_xy(fishpop(fish_id_temp, 2), fishpop(fish_id_temp, 3),
                                         extent, dimensions, true);

    // calculate growth in length and weight
    double growth_length = pop_k / (365.0 * 24.0 * 60.0) * min_per_i *
      (pop_linf - fishpop(fish_id_temp, 5));

    // length intial + change in length^b
    double growth_weight = pop_a * (std::pow((fishpop(fish_id_temp, 5) + growth_length), pop_b) -
                                    std::pow(fishpop(fish_id_temp, 5), pop_b));

    // consumption req based on growth in weight + metabolic costs based on weight + n required
    double consumption_require = ((growth_weight + fishpop(fish_id_temp, 8) *
                                  fishpop(fish_id_temp, 6)) / 0.55) * pop_n_body;

    // check mortality behavior 3 (foraging, reserves + detritus available)
    if (fishpop(fish_id_temp, 11) == 3.0) {

      // individual dies because consumption requirements cannot be met by
      // detritus in cell and reserves
      if (consumption_require > (seafloor(cell_id_temp, 5) + fishpop(fish_id_temp, 9))) {

        rcpp_reincarnate(fishpop, fishpop_track, fish_id_temp,
                         seafloor, extent, dimensions,
                         pop_linf, pop_n_body, pop_reserves_max,
                         "consumption");

        continue;

      // individual grows because consumption requirements can be met
      } else {

        //  increase age (60 min * 24 h = 1440 min/day)
        fishpop(fish_id_temp, 1) += 1.0; // (min_per_i / 1440.0);

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
          double consumption_limit = pop_reserves_consump * fishpop(fish_id_temp, 10);

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

        rcpp_reincarnate(fishpop, fishpop_track, fish_id_temp,
                         seafloor, extent, dimensions,
                         pop_linf, pop_n_body, pop_reserves_max,
                         "consumption");

        continue;

      // individual grows because consumption requirements can be met
      } else {

        // increase age (60 min * 24 h = 1440 min/day)
        fishpop(fish_id_temp, 1) += 1.0; // (min_per_i / 1440.0);

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
    fishpop(fish_id_temp, 10) = fishpop(fish_id_temp, 6) * pop_n_body * pop_reserves_max;

  }
}
