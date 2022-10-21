#include <Rcpp.h>

#include "rcpp_fishpop_growth.h"
#include "rcpp_which.h"
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
//' @param fishpop_attr Matrix with id, threshold of pop_reserves_thres_mean, and pop_reserves_consump values
//' @param seafloor Matrix with seafloor values.
//' @param pop_k,pop_linf,pop_a,pop_b Numeric with parameters.
//' @param pop_n_body,pop_reserves_max,min_per_i Numeric with parameters.
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
                         Rcpp::NumericMatrix fishpop_attr, Rcpp::NumericMatrix seafloor,
                         double pop_k, double pop_linf, double pop_a, double pop_b,
                         double pop_n_body, double pop_reserves_max,
                         Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                         double min_per_i) {

  // MH: Lots of code repetition here.

  // create random order of fish id because detritus can "run out"
  Rcpp::NumericVector row_id = rcpp_shuffle(fishpop(_, 0), false);

  // loop through all fish ids
  for (int i = 0; i < row_id.length(); i++) {

    // use Rcpp indexing counter of current loop iteration
    int row_id_temp = row_id[i] - 1;

    // get current row id
    int id_attr = rcpp_which(fishpop(row_id_temp, 0), fishpop_attr(_, 0));

    // get cell id of current individual
    int cell_id_temp = rcpp_cell_from_xy(fishpop(row_id_temp, 2), fishpop(row_id_temp, 3),
                                         extent, dimensions, true);

    // calculate growth in length and weight
    double growth_length = pop_k / (365.0 * 24.0 * 60.0) * min_per_i *
      (pop_linf - fishpop(row_id_temp, 5));

    // length intial + change in length^b
    double growth_weight = pop_a * (std::pow((fishpop(row_id_temp, 5) + growth_length), pop_b) -
                                    std::pow(fishpop(row_id_temp, 5), pop_b));

    // consumption req based on growth in weight + metabolic costs based on weight + n required
    double consumption_require = ((growth_weight + fishpop(row_id_temp, 8) *
                                  fishpop(row_id_temp, 6)) / 0.55) * pop_n_body;

    // enough nutrients for individual growth
    if ((seafloor(cell_id_temp, 5) + fishpop(row_id_temp, 9)) >= consumption_require) {

      // increase age (60 min * 24 h = 1440 min/day)
      fishpop(row_id_temp, 1) += 1.0; // (min_per_i / 1440.0);

      // increase fish dimensions length
      fishpop(row_id_temp, 5) += growth_length;

      // increase fish dimensions weight
      fishpop(row_id_temp, 6) += growth_weight;

      // update reserves_max
      fishpop(row_id_temp, 10) = fishpop(row_id_temp, 6) * pop_n_body * pop_reserves_max;

      // calc non-used consumption (excretion)
      double excretion = (consumption_require - (growth_weight * pop_n_body));

      // add non-used consumption to nutrient pool
      seafloor(cell_id_temp, 4) += excretion;

      // track excretion cell
      seafloor(cell_id_temp, 14) += excretion;

      // track excretion fish
      fishpop(row_id_temp, 13) += excretion;

      // behavior 3: individuals are foraging
      if (fishpop(row_id_temp, 11) == 3.0) {

        // detritus pool is big enough to fill reserves
        if (seafloor(cell_id_temp, 5) > consumption_require) {

          // calculate difference between reserves max and current reserves
          double nutrients_diff = fishpop(row_id_temp, 10) - fishpop(row_id_temp, 9);

          // calculate max amount that fish can consume from threshold matrix
          double consumption_max = fishpop_attr(id_attr, 2) * fishpop(row_id_temp, 10);

          // calculate amount fish would consume if available
          double consumption_wanted = std::min(nutrients_diff, consumption_max);

          // calculate remaining detritus after consumption for bioenergetics
          double detritus_additional = seafloor(cell_id_temp, 5) - consumption_require;

          // calculate amount that is consumed to fill reserves
          double consumption_reserve = std::min(detritus_additional, consumption_wanted);

          // increase reserves
          fishpop(row_id_temp, 9) += consumption_reserve;

          // reduce detritus pool by reserves
          seafloor(cell_id_temp, 5) -= (consumption_require + consumption_reserve);

          // track consumption cell
          seafloor(cell_id_temp, 13) += (consumption_require + consumption_reserve);

          // track consumption fish
          fishpop(row_id_temp, 12) += (consumption_require + consumption_reserve);

        // detritus pool is not big enough to me consumption requirements
        } else {

          // reduced reserves because there was not enough detritus in cell
          fishpop(row_id_temp, 9) -= (consumption_require - seafloor(cell_id_temp, 5));

          // set detritus pool to zero
          seafloor(cell_id_temp, 5) = 0.0;

          // track consumption cell
          seafloor(cell_id_temp, 13) += seafloor(cell_id_temp, 5);

          // track consumption fish
          fishpop(row_id_temp, 12) += seafloor(cell_id_temp, 5);

        }

      // behavior 1/2: individuals use reserves only if possible
      } else {

        // reserves are big enough to meet consumption requirements
        if (fishpop(row_id_temp, 9) > consumption_require) {

          fishpop(row_id_temp, 9) -= consumption_require;

        // reserves not big enough to meet consumption requirements
        } else {

          // reduced detritus pool because there were not enough reserves
          seafloor(cell_id_temp, 5) -= (consumption_require - fishpop(row_id_temp, 9));

          // use all reserves
          fishpop(row_id_temp, 9) = 0.0;

          // track consumption cell
          seafloor(cell_id_temp, 13) += (consumption_require - fishpop(row_id_temp, 9));

          // track consumption fish
          fishpop(row_id_temp, 12) += (consumption_require - fishpop(row_id_temp, 9));

        }
      }

    // individual dies because consumption requirements cannot be met by detritus and reserves
    } else {

      rcpp_reincarnate(fishpop, fishpop_track, row_id_temp,
                       seafloor, extent, dimensions, pop_n_body, pop_reserves_max,
                       "consumption");

    }
  }
}
