#include <Rcpp.h>

#include "rcpp_fishpop_growth.h"
#include "rcpp_cell_from_xy.h"
#include "rcpp_reincarnate.h"
#include "rcpp_shuffle.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_fishpop_growth
//'
//' @description
//' Rcpp simulate fishpop growth.
//'
//' @param fishpop,fishpop_track Matrix with fishpop values and starting population.
//' @param seafloor Matrix with seafloor values.
//' @param pop_k,pop_linf,pop_a,pop_b Vector with parameters.
//' @param pop_n_body,pop_reserves_max,min_per_i Vector with parameters.
//' @param pop_reserves_consump Vector with consumption limit to fill reserves each time step.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//'
//' @details
//' Function to simulate consumption, mortality, growth and excretion of fish
//' individuals.
//'
//' Each fish individual has to follow the von Bertalanffy growth curve (Froese and
//' Pauly 2019) and the corresponding amount of nutrients for this are calculated based
//' on a bioenergetics model (Allgeier et al. 2020). The nutrients are consumed
//' from the detritus pool. If the available amount is not big enough, fish can
//' either use their reserves. If there are no reserves, individuals die. Last,
//' if the detritus pool is big enough, individuals can additionally fill up their
//' reserves.
//'
//' If individuals are acting accordingly to movement state 1 or 2 (only for
//' \code{movement = 'behav'}), the consumption requirement must be met by the
//' reserves only.
//'
//' If \code{0 < pop_reserves_consump < 1}, only a ratio of the \code{pop_reserves_max}
//' can be consumed each time step.
//'
//' @references
//' Allgeier, J.E., Cline, T.J., Walsworth, T.E., Wathen, G., Layman, C.A.,
//' Schindler, D.E., 2020. Individual behavior drives ecosystem function and the impacts of
//' harvest. Sci. Adv. 6, eaax8329. <https://doi.org/10.1126/sciadv.aax8329>
//'
//' Froese, R., Pauly, D., 2019. FishBase. World Wide Web electronic publication
//' [WWW Document]. <www.fishbase.org>
//'
//' @return void
//'
//' @aliases rcpp_fishpop_growth
//' @rdname rcpp_fishpop_growth
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                         Rcpp::NumericMatrix seafloor,
                         Rcpp::NumericVector pop_k, Rcpp::NumericVector pop_linf,
                         Rcpp::NumericVector pop_a, Rcpp::NumericVector pop_b,
                         Rcpp::NumericVector pop_n_body, Rcpp::NumericVector pop_reserves_max,
                         Rcpp::NumericVector pop_reserves_consump,
                         Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                         double min_per_i) {

  // MH: Lots of code repetition here.

  // create random order of fish id because detritus can "run out"
  Rcpp::NumericVector row_id = rcpp_shuffle(fishpop(_, 0), false);

  // loop through all fish ids
  for (int i = 0; i < row_id.length(); i++) {

    // use Rcpp indexing counter of current loop iteration
    int row_id_temp = row_id[i] - 1;

    // get current species id
    int species_temp = fishpop(row_id_temp, 1) - 1;

    // MH: This is an example of printing (uncomment next line)
    // Rcout << "i:" << i << "; species:" << species_temp << std::endl;

    // get cell id of current individual
    int cell_id_temp = rcpp_cell_from_xy(fishpop(row_id_temp, 3), fishpop(row_id_temp, 4),
                                         extent, dimensions, true);

    // calculate growth in length and weight
    double growth_length = pop_k[species_temp] / (365.0 * 24.0 * 60.0) * min_per_i *
      (pop_linf[species_temp] - fishpop(row_id_temp, 6));

    // length intial + change in length^b
    double growth_weight = pop_a[species_temp] *
      (std::pow((fishpop(row_id_temp, 6) + growth_length), pop_b[species_temp]) -
      std::pow(fishpop(row_id_temp, 6), pop_b[species_temp]));

    // consumption req based on growth in weight + metabolic costs based on weight + n required
    double consumption_require = ((growth_weight + fishpop(row_id_temp, 9) *
                                  fishpop(row_id_temp, 7)) / 0.55) * pop_n_body[species_temp];

    // enough nutrients for individual growth
    if ((seafloor(cell_id_temp, 5) + fishpop(row_id_temp, 10)) >= consumption_require) {

      // increase age (60 min * 24 h = 1440 min/day)
      fishpop(row_id_temp, 2) += 1.0; // (min_per_i / 1440.0);

      // increase fish dimensions length
      fishpop(row_id_temp, 6) += growth_length;

      // increase fish dimensions weight
      fishpop(row_id_temp, 7) += growth_weight;

      //establish temporary variable for reserve proportional change upon birth
      double reserves_max_temp = fishpop(row_id_temp, 11);

      // update reserves_max
      fishpop(row_id_temp, 11) = fishpop(row_id_temp, 7) * pop_n_body[species_temp] * pop_reserves_max[species_temp];

      if (fishpop(row_id_temp, 2) == 1) {
        fishpop(row_id_temp, 10) = fishpop(row_id_temp, 10) * (fishpop(row_id_temp, 11) / reserves_max_temp);
       }

      // calc non-used consumption (excretion)
      double excretion = (consumption_require - (growth_weight * pop_n_body[species_temp]));

      // add non-used consumption to nutrient pool
      seafloor(cell_id_temp, 4) += excretion;

      // track excretion cell
      seafloor(cell_id_temp, 14) += excretion;

      // track excretion fish
      fishpop(row_id_temp, 14) += excretion;

      // behavior 3: individuals are foraging
      if (fishpop(row_id_temp, 12) == 3.0 || species_temp == 1) {

        // always on reef, detritus pool should almost always be big enough
        // noticing that consumption is negative on result$fishpop, only starts consuming at 0

        // detritus pool is big enough to fill reserves
        if (seafloor(cell_id_temp, 5) > consumption_require) {

          // calculate difference between reserves max and current reserves
          double nutrients_diff;
          // ensures nutrients_diff will not be negative --> still only eats when 0
          if (fishpop(row_id_temp, 11) > fishpop(row_id_temp, 10)) {
            nutrients_diff = fishpop(row_id_temp, 11) - fishpop(row_id_temp, 10);
            // would normally be positive, but can be negative if value current is higher than reserves (like upon birth)
            // nutrients_diff would be equal to max when 0
          }
          else {
            nutrients_diff = 0;
          }

          // calculate max amount that fish can consume
          double consumption_limit = pop_reserves_consump[species_temp] * fishpop(row_id_temp, 11);

          // calculate max amount that fish can consume
          double consumption_reserve = std::min(nutrients_diff, consumption_limit);

          // consupmtion_reserve will be negative if current reserves are higher than max (often happens at spawn)
          // results in an actual decrease in the amount of reserves --> does not explain consistent zeroes though
          // consumption_reserve will be consumption_limit always

          // calculate max amount that is present in cell
          consumption_reserve = std::min(consumption_reserve, seafloor(cell_id_temp, 5));
          // unknown whether consumption limit will be higher than seafloor detritus, likely lower

          // increase reserves
          fishpop(row_id_temp, 10) += consumption_reserve;
          // if consumption_reserve is negative, reserves will decrease
          // consumption_reserve should be large when reserves are zero, means large increase in reserves

          // reduce detritus pool by reserves
          seafloor(cell_id_temp, 5) -= (consumption_require + consumption_reserve);

          // track consumption cell
          seafloor(cell_id_temp, 13) += (consumption_require + consumption_reserve);

          // track consumption fish
          fishpop(row_id_temp, 13) += (consumption_require + consumption_reserve);

        // detritus pool is not big enough to me consumption requirements
        } else {

          // reduced reserves because there was not enough detritus in cell
          fishpop(row_id_temp, 10) -= (consumption_require - seafloor(cell_id_temp, 5));

          // set detritus pool to zero
          seafloor(cell_id_temp, 5) = 0.0;

          // track consumption cell
          seafloor(cell_id_temp, 13) += seafloor(cell_id_temp, 5);

          // track consumption fish
          fishpop(row_id_temp, 13) += seafloor(cell_id_temp, 5);

        }

      // behavior 1/2: individuals use reserves only if possible
      } else {

        // reserves are big enough to meet consumption requirements
        if (fishpop(row_id_temp, 10) > consumption_require) {

          fishpop(row_id_temp, 10) -= consumption_require;

        // reserves not big enough to meet consumption requirements
        } else {

          // reduced detritus pool because there were not enough reserves
          seafloor(cell_id_temp, 5) -= (consumption_require - fishpop(row_id_temp, 10));

          // use all reserves
          fishpop(row_id_temp, 10) = 0.0;

          // track consumption cell
          seafloor(cell_id_temp, 13) += (consumption_require - fishpop(row_id_temp, 10));

          // track consumption fish
          fishpop(row_id_temp, 13) += (consumption_require - fishpop(row_id_temp, 10));

        }
      }

    // individual dies because consumption requirements cannot be met by detritus and reserves
    } else {

      rcpp_reincarnate(fishpop, fishpop_track, row_id_temp, seafloor, extent, dimensions,
                       pop_linf[species_temp], pop_n_body[species_temp], pop_reserves_max[species_temp],
                       "consumption");

    }
  }
}
