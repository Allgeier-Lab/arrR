#include "rcpp_run_simulation.h"
#include "rcpp_nutr_input.h"
#include "rcpp_seagrass_growth.h"
#include "rcpp_mineralization.h"
#include "rcpp_move_rand.h"
#include "rcpp_move_behav.h"
#include "rcpp_respiration.h"
#include "rcpp_fishpop_growth.h"
#include "rcpp_mortality.h"
#include "rcpp_diffuse_values.h"
#include "rcpp_nutr_output.h"

//' rcpp_run_simulation
//'
//' @description Rcpp run simulation
//'
//' @param seafloor,fishpop Matrix with seafloor and fishpop data.
//' @param seafloor_track,fishpop_track List with entry for each saving timestep.
//' @param parameters List with parameters.
//' @param pop_n Integer with number of individuals.
//' @param movement String specifing movement algorithm. Either 'rand', 'attr' or 'behav'.
//' @param max_dist Double with maximum movement distance.
//' @param pop_thres_reserves Vector with threshold of pop_max_reserves to drain prior to foraging.
//' @param cells_reef, coords_reef Vector and Matrix with ID and coords of reef cells.
//' @param cell_adj Matrix with cell adjacencies.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//' @param nutr_input Vector with amount of nutrient input each timestep.
//' @param max_i Integer with maximum number of simulation timesteps.
//' @param min_per_i Integer to specify minutes per i.
//' @param save_each Numeric how often data should be saved to return.
//' @param seagrass_each Integer how often (each i * x) seagrass dynamics will be simulated.
//' @param burn_in Numeric with timesteps used to burn in.
//' @param verbose If TRUE, progress reports are printed.
//'
//' @details
//' Run model
//'
//' @references
//' Add reference
//'
//' @return void
//'
//' @aliases rcpp_run_simulation
//' @rdname rcpp_run_simulation
//'
//' @export
// [[Rcpp::export]]
void rcpp_run_simulation(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix fishpop,
                         List seafloor_track, List fishpop_track, List parameters,
                         int pop_n, String movement, double max_dist, Rcpp::NumericVector pop_thres_reserves,
                         Rcpp::NumericVector cells_reef, Rcpp::NumericMatrix coords_reef,
                         Rcpp::NumericMatrix cell_adj, Rcpp::NumericVector extent, Rcpp::NumericVector dimensions,
                         Rcpp::NumericVector nutr_input,
                         int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in,
                         bool verbose) {

  // save input data in tracking list
  seafloor_track[0] = Rcpp::clone(seafloor);

  fishpop_track[0] = Rcpp::clone(fishpop);

  // calc time_frac for rcpp_seagrass_growth
  double time_frac = (min_per_i / 60.0) * seagrass_each;

  // setup progress
  Progress progress(max_i, verbose);

  // run simulation
  for (int i = 0; i < max_i; i++) {

    // check abort of function
    if (Progress::check_abort()) {

      Rcpp::stop("Stopped by user.");

    }

    // simulate nutrient input
    if (nutr_input(i) != 0.0) {

      // simulate nutrient input
      rcpp_nutr_input(seafloor, nutr_input(i));

    }

    // simulate seagrass only each seagrass_each iterations
    if ((i * min_per_i) % (min_per_i * seagrass_each) == 0) {

      // simulate seagrass growth
      rcpp_seagrass_growth(seafloor, cells_reef,
                           as<double>(parameters["bg_v_max"]), as<double>(parameters["bg_k_m"]), as<double>(parameters["bg_gamma"]),
                           as<double>(parameters["ag_v_max"]), as<double>(parameters["ag_k_m"]), as<double>(parameters["ag_gamma"]),
                           as<double>(parameters["bg_biomass_max"]), as<double>(parameters["bg_biomass_min"]),
                           as<double>(parameters["ag_biomass_max"]), as<double>(parameters["ag_biomass_min"]),
                           as<double>(parameters["seagrass_thres"]), as<double>(parameters["seagrass_slope"]),
                           as<double>(parameters["seagrass_slough"]), time_frac);

      // simulate mineralization (detritus to nutrients pool)
      rcpp_mineralization(seafloor, as<double>(parameters["detritus_fish_decomp"]),
                          as<double>(parameters["detritus_mineralization"]));

    }

    // fish indiviuals are present and i above burn_in
    if (i >= burn_in && pop_n != 0) {

      // calculate new coordinates and activity
      // MH: wrap into rcpp_move_wrap for cleaner code?

      // random movement
      if (movement == "rand") {

        rcpp_move_rand(fishpop, coords_reef,
                       as<double>(parameters["move_mean"]), as<double>(parameters["move_var"]),
                       as<double>(parameters["move_visibility"]), max_dist, FALSE, extent, dimensions);

      // attracted movement
      } else if (movement == "attr") {

        rcpp_move_rand(fishpop, coords_reef,
                       as<double>(parameters["move_mean"]), as<double>(parameters["move_var"]),
                       as<double>(parameters["move_visibility"]), max_dist, TRUE, extent, dimensions);

      // behaviour movement
      } else if (movement == "behav") {

        rcpp_move_behav(fishpop, coords_reef, pop_thres_reserves,
                        as<double>(parameters["move_mean"]), as<double>(parameters["move_var"]),
                        as<double>(parameters["move_reef"]), as<double>(parameters["move_border"]),
                        as<double>(parameters["move_return"]), max_dist, extent, dimensions);

      // throw error
      } else {

        throw std::range_error("Please select allowed 'movement' vale");

      }

      // simulate fish respiration (26°C is mean water temperature in the Bahamas)
      rcpp_respiration(fishpop,
                       as<double>(parameters["resp_intercept"]), as<double>(parameters["resp_slope"]),
                       as<double>(parameters["resp_temp_low"]), as<double>(parameters["resp_temp_max"]),
                       as<double>(parameters["resp_temp_optm"]), 26.0, min_per_i);

      // simulate fishpop growth and including change of seafloor pools
      rcpp_fishpop_growth(fishpop, fishpop_track[0], seafloor,
                          as<double>(parameters["pop_k"]), as<double>(parameters["pop_linf"]),
                          as<double>(parameters["pop_a"]), as<double>(parameters["pop_b"]),
                          as<double>(parameters["pop_n_body"]), as<double>(parameters["pop_max_reserves"]),
                          as<double>(parameters["pop_consumption_prop"]), extent, dimensions, min_per_i);

      // simulate mortality
      rcpp_mortality(fishpop, fishpop_track[0], seafloor,
                     as<double>(parameters["pop_linf"]), as<double>(parameters["pop_n_body"]),
                     as<double>(parameters["pop_max_reserves"]), extent, dimensions);

    }

    // diffuse values between neighbors
    rcpp_diffuse_values(seafloor, cell_adj,
                        as<double>(parameters["nutrients_diffusion"]), as<double>(parameters["detritus_diffusion"]),
                        as<double>(parameters["detritus_fish_diffusion"]));

    // remove nutrients from cells if output parameter > 0
    if(as<double>(parameters["nutrients_output"]) > 0.0) {

      rcpp_nutr_output(seafloor, as<double>(parameters["nutrients_output"]));

    }

    // update tracking list
    if (i % save_each == 0) {

      seafloor_track[i / save_each + 1] = Rcpp::clone(seafloor);

      fishpop_track[i / save_each + 1] = Rcpp::clone(fishpop);

    }

    // update progress bar
    progress.increment();

  }
}

/*** R
rcpp_run_simulation(seafloor = seafloor_values, fishpop = fishpop_values,
                    seafloor_track = seafloor_track, fishpop_track = fishpop_track,
                    parameters = parameters, pop_n = starting_values$pop_n,
                    movement = movement, max_dist = max_dist, pop_thres_reserves = pop_thres_reserves,
                    cells_reef = cells_reef, coords_reef = coords_reef, cell_adj = cell_adj,
                    extent = extent, dimensions = dimensions, nutr_input = nutr_input,
                    max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                    seagrass_each = seagrass_each, burn_in = burn_in, verbose = verbose)
*/
