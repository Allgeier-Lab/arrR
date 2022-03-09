// [[Rcpp::depends(RcppProgress)]]

#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>

#include "rcpp_simulate.h"
#include "rcpp_get_reef.h"
#include "rcpp_get_adjacencies.h"
#include "rcpp_get_max_dist.h"
#include "rcpp_rnorm.h"
#include "rcpp_nutr_input.h"
#include "rcpp_seagrass_growth.h"
#include "rcpp_mineralization.h"
#include "rcpp_move_wrap.h"
#include "rcpp_respiration.h"
#include "rcpp_fishpop_growth.h"
#include "rcpp_mortality.h"
#include "rcpp_diffuse_values.h"
#include "rcpp_nutr_output.h"

using namespace Rcpp;

//' rcpp_simulate
//'
//' @description
//' Rcpp run simulation.
//'
//' @param seafloor,fishpop Matrix with seafloor and fishpop data.
//' @param nutrients_input Vector with amount of nutrient input each time step.
//' @param seafloor_track,fishpop_track List with entry for each saving time step.
//' @param parameters List with parameters.
//' @param movement String specifing movement algorithm.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//' @param max_i Integer with maximum number of simulation time steps.
//' @param min_per_i Integer to specify minutes per i.
//' @param save_each Numeric how often data should be saved to return.
//' @param seagrass_each Integer how often (each i * x) seagrass dynamics will be simulated.
//' @param burn_in Numeric with time steps used to burn in.
//' @param verbose Logical if TRUE, progress reports are printed.
//'
//' @details
//' The functions is a 'wrapper' around the following sub-processes: (i) nutrient input,
//' (ii) seagrass growth, (iii) detritus mineralization, (iv) movement of individuals,
//' (v) respiration of individuals, (vi) growth of individuals, (vii) mortality of individuals,
//' (viii) diffusion of nutrients/detritus, and ix) nutrient output.
//'
//' @references
//' For a detailed model description, please see Esquivel, K.E., Hesselbarth, M.H.K.,
//' Allgeier, J.E., In Press. Mechanistic support for increased primary production
//' around artificial reefs. Ecological Applications. v0.0
//'
//' @return void
//'
//' @aliases rcpp_simulate
//' @rdname rcpp_simulate
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_simulate(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix fishpop, Rcpp::NumericVector nutrients_input,
                   Rcpp::List seafloor_track, Rcpp::List fishpop_track,
                   Rcpp::List parameters, Rcpp::String movement,
                   Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                   int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in,
                   bool verbose) {

  // init flags to run processes //

  // flag if diffusion needs to be run
  bool flag_diffuse = (as<double>(parameters["nutrients_diffusion"]) > 0.0) ||
    (as<double>(parameters["detritus_diffusion"]) > 0.0) ||
    (as<double>(parameters["detritus_fish_diffusion"])) > 0.0;

  // get input flag
  bool flag_input = Rcpp::sum(nutrients_input) > 0.0;

  // flag if output needs to be run
  bool flag_output = (as<double>(parameters["nutrients_loss"]) > 0.0) ||
    (as<double>(parameters["detritus_loss"]) > 0.0);

  // flag if fishpop is present
  bool flag_fishpop = fishpop.nrow() > 0;

  // init reef coords //

  // get reef coords matrix
  Rcpp::NumericMatrix coords_reef = rcpp_get_reef(seafloor);

  // get cell neighborhoods
  Rcpp::IntegerMatrix cell_adj = rcpp_get_adjacencies(dimensions);

  // setup nutrients input //

  // only one value present
  if (nutrients_input.length() == 1) {

    nutrients_input = Rcpp::rep(nutrients_input, max_i);

  }

  // init fishpop //

  // init matrix for reserves threshold
  Rcpp::NumericMatrix fishpop_attr(fishpop.nrow(), 2);

  // add id column of fish
  fishpop_attr(_, 0) = fishpop(_, 0);

  // init double for maximum movement distance
  double max_dist = 0.0;

  // fishpop is present
  if (flag_fishpop) {

    // get maximum movement distance
    max_dist = rcpp_get_max_dist(movement, parameters, 1000000);

    if (movement == "behav") {

      // create random reserves threshold value
      for (int i = 0; i < fishpop.nrow(); i++) {

        fishpop_attr(i, 1) = rcpp_rnorm(parameters["pop_reserves_thres_mean"],
                                        parameters["pop_reserves_thres_var"], 0.0, 1.0);

      }
    }
  }

  // calc time_frac for rcpp_seagrass_growth
  double time_frac = (min_per_i / 60.0) * seagrass_each;

  // save original data //

  // save input data in tracking list
  seafloor_track[0] = Rcpp::clone(seafloor);

  fishpop_track[0] = Rcpp::clone(fishpop);

  // setup progress bar
  Progress progress(max_i, verbose);

  // run simulation
  for (int i = 1; i <= max_i; i++) {

    // check abort of function
    if (Progress::check_abort()) {

      Rcpp::stop("Stopped by user.");

    }

        // simulate nutrient input if present
    if (flag_input && (nutrients_input[i] > 0.0)) {

      rcpp_nutr_input(seafloor, nutrients_input[i]);

    }

    // simulate seagrass only each seagrass_each iterations
    if ((i % seagrass_each) == 0) {

      // simulate seagrass growth
      rcpp_seagrass_growth(seafloor, parameters["bg_v_max"], parameters["bg_k_m"], parameters["bg_gamma"],
                           parameters["ag_v_max"], parameters["ag_k_m"], parameters["ag_gamma"],
                           parameters["bg_biomass_max"], parameters["bg_biomass_min"],
                           parameters["ag_biomass_max"], parameters["ag_biomass_min"],
                           parameters["seagrass_thres"], parameters["seagrass_slope"],
                           parameters["seagrass_slough"], time_frac);

      // simulate mineralization (detritus to nutrients pool)
      rcpp_mineralization(seafloor, parameters["detritus_mineralization"],
                          parameters["detritus_fish_decomp"]);

    }

    // fish individuals are present and i above burn_in
    if ((i > burn_in) && flag_fishpop) {

      // calculate new coordinates and activity
      rcpp_move_wrap(fishpop, fishpop_attr, movement,
                     parameters["move_mean"], parameters["move_var"],
                     parameters["move_reef"], parameters["move_border"],
                     parameters["move_return"], max_dist, coords_reef, extent, dimensions);

      // simulate fish respiration (26°C is mean water temperature in the Bahamas)
      rcpp_respiration(fishpop, parameters["resp_intercept"], parameters["resp_slope"],
                       parameters["resp_temp_low"], parameters["resp_temp_max"],
                       parameters["resp_temp_optm"], 26.0, min_per_i);

      // simulate fishpop growth and including change of seafloor pools
      rcpp_fishpop_growth(fishpop, fishpop_track[0], seafloor,
                          parameters["pop_k"], parameters["pop_linf"],
                          parameters["pop_a"], parameters["pop_b"],
                          parameters["pop_n_body"], parameters["pop_reserves_max"],
                          parameters["pop_reserves_consump"], extent, dimensions, min_per_i);

      // simulate mortality
      rcpp_mortality(fishpop, fishpop_track[0], seafloor,
                     parameters["pop_linf"], parameters["pop_n_body"],
                     parameters["pop_reserves_max"], extent, dimensions);

    }

    // only diffuse if all parameters larger than zero
    if (flag_diffuse) {

      // diffuse values between neighbors
      rcpp_diffuse_values(seafloor, cell_adj,
                          parameters["nutrients_diffusion"], parameters["detritus_diffusion"],
                          parameters["detritus_fish_diffusion"]);

    }

    // remove nutrients from cells if output parameter > 0
    if (flag_output) {

      rcpp_nutr_output(seafloor, parameters["nutrients_loss"], parameters["detritus_loss"]);

    }

    // update tracking list
    if ((i % save_each) == 0) {

      seafloor_track[i / save_each] = Rcpp::clone(seafloor);

      fishpop_track[i / save_each] = Rcpp::clone(fishpop);

    }

    // update progress bar
    progress.increment();

  }
}
