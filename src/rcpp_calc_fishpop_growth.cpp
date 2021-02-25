#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_calc_fishpop_growth
//'
//' @description Rcpp calc growth
//'
//' @param fishpop,fishpop_track Matrix with fishpop values and starting population.
//' @param seafloor Matrix with seafloor values.
//' @param fish_id,cell_id Vector with id of fish and corresponding cell ids.
//' @param pop_k,pop_linf,pop_a,pop_b Numeric with parameters.
//' @param pop_n_body,pop_max_reserves,pop_want_reserves,min_per_i Numeric with parameters.
//'
//' @details
//' Rcpp implementation to calculate growth of fish individuals.
//' "KSM" notes from Katrina to help understand code
//' "Q" questions Katrina has for Max
//' "C" code to add
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
                              double pop_k, double pop_linf,
                              double pop_a, double pop_b,
                              double pop_n_body, double pop_max_reserves, double pop_want_reserves,
                              double min_per_i) {

  // loop through all fish ids
  for (int i = 0; i < fish_id.length(); i++) {

    // create counter for temp fish id
    // KSM: sets up a temporary order of fish ID, so that Fish 1 does not get all of the resources in each patch
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

    // calculate amount of available resources
    // KSM: available resources = resources (detritus pool) per cell + fish reserves (per cell)
    double available_resources = seafloor(cell_id_temp, 5) + fishpop(fish_id_temp, 7);

    // KSM: ADDING IF,ELSE STATEMENTS HERE

    // KSM: if reserves match max_reserves,
    // C: if fish_pop(fish_id_temp, 8) = fishpop(fish_id_temp, 7) {

    // KSM: set consumption to zero
    // C:  seafloor(cell_id_temp, 13) = 0;

    // KSM: reduce reserves to meet consumption_req
    // C: fishpop(fish_id_temp, 7) -= consumption_req;

    // Q: do I need add in excretion argument here or does excretion argument in line 233 work?
    // MH: I would say the one in l233 works, you just need to make sure with all the
    // if-else statement now it can be actually reached

    // KSM: else, check if individual feeds or dies (based on reserves, detritus, and consumption_req)
    // C: } else {

    // KSM: reserves < 3/4 of max_reserves, fish needs to eat (or dies)
    // C: fishpop(fish_id_temp, 7) < 0.75 * fish_pop(fish_id_temp, 8);

    // Q: can this go directly into all other if, else statements?
    // MH: Not sure what you mean by this? Maybe it helps to draw a flow chart with
    // all if-else?

    // individual dies because consumption requirements cannot be met
    // KSM: if consumption requirements are greater than available resources per cell, fish dies

    if (consumption_req > available_resources) {

      // save current original coordinates
      double x_coord = fishpop(fish_id_temp, 2);

      double y_coord = fishpop(fish_id_temp, 3);

      // save current mortality counter
      int died_consumption = fishpop(fish_id_temp, 11);

      int died_background = fishpop(fish_id_temp, 12);

      // calculate increase in fish mass including reserves
      // KSM: puts nutrients from dead fish into detrital pool
      // KSM: mass_difference = weight - weight specific nutrient content + fish reserves
      double mass_diff = (fishpop(fish_id_temp, 6) - fishpop_track(fish_id_temp, 6)) * pop_n_body +
        fishpop(fish_id_temp, 7);

      // add to dead detritus pool
      seafloor(cell_id_temp, 6) += mass_diff;

      // create new individual
      // KSM: access all columns of fish_id_temp DF
      fishpop(fish_id_temp, _) = fishpop_track(fish_id_temp, _);

      // keep old coordinates
      // KSM: put new fish back in place of dead fish
      fishpop(fish_id_temp, 2) = x_coord;

      fishpop(fish_id_temp, 3) = y_coord;

      // calculate wanted reserves
      // KSM: calculate new reserves for new fish of new size
      double reserves_wanted = pop_n_body * fishpop(fish_id_temp, 6) * pop_want_reserves;

      // detritus pool is smaller than wanted reserves, detritus pool is fully used
      if (reserves_wanted >= seafloor(cell_id_temp, 5)) {

        // use pool completely
        // KSM: fish fully consumes detritus pool in cell
        fishpop(fish_id_temp, 7) = seafloor(cell_id_temp, 5);

        // set pool to zero
        // KSM: cell goes to 0
        seafloor(cell_id_temp, 5) = 0;

      // detritus pool is larger than what is wanted, so only subset is used
      // KSM: otherwise, if detritus pool is large enough, only a subset is used by fish
      } else {

        // wanted reserves can be filled completely
        fishpop(fish_id_temp, 7) = reserves_wanted;

        // reduced detritus pool by wanted reserves
        // KSM: remove consumed detritus from cell to match reserve needs
        seafloor(cell_id_temp, 5) -= reserves_wanted;

      }

      // update mortality counter
      // KSM: add to died_consumption when fish dies because consumption needs not met
      fishpop(fish_id_temp, 11) = died_consumption + 1;

      fishpop(fish_id_temp, 12) = died_background;

    // individual grows because consumption requirements can be met
    // KSM: end of 'if' statement (fish dies), otherwise fish grows
    } else {

      //  increase age (60 min * 24 h = 1440 min/day)
      // KSM: current age + time past
      fishpop(fish_id_temp, 1) += (min_per_i / 1440.0);

      // increase fish dimensions length
      // KSM: current length + growth in length
      fishpop(fish_id_temp, 5) += growth_length;

      // increase fish dimensions weight
      //KSM: current weight + growth in weight
      fishpop(fish_id_temp, 6) += growth_weight;

      // update max reserves based on weight
      //KSM: reserves_max = weight * size-specific n requirements * max amt of reserves in relation to body size
      fishpop(fish_id_temp, 8) = fishpop(fish_id_temp, 6) * pop_n_body * pop_max_reserves;

      // calculate reserves difference
      // KSM: difference in reserves = reserves_max - current reserves
      // KSM: essentially how much reserves are needed to have full reserves
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
          // KSM: detritus pool in cell = current detritus pool - (current requirements based on size + how much fish needs to fill reserves)
          seafloor(cell_id_temp, 5) -= (consumption_req + reserves_diff);

          // track consumption
          // KSM: how much detritus is being consumed in a cell based on consumption requirements and reserves needed
          seafloor(cell_id_temp, 13) += consumption_req + reserves_diff;

        // reserves cannot be filled completely by nutrient pool
        } else {

          // add all nutrients that are left
          // KSM: reserves still needed
          fishpop(fish_id_temp, 7) += nutrients_left;

          // set detritus pool to zero
          // KSM completely drained detritus pool in cell
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

      // calc non-used consumption (excretion)
      double excretion_temp = consumption_req - (growth_weight * pop_n_body);

      // track excretion
      seafloor(cell_id_temp, 14) += excretion_temp;

      // add non-used consumption to nutrient pool
      seafloor(cell_id_temp, 4) += excretion_temp;

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
                         min_per_i = min_per_i)
*/
