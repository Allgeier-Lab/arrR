// [[Rcpp::depends(RcppProgress)]]

#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "rcpp_sim_processes.h"
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

//' rcpp_sim_processes
//'
//' @description
//' Rcpp run simulation.
//'
//' @param seafloor,fishpop Matrix with seafloor and fishpop data.
//' @param seafloor_track,fishpop_track List with entry for each saving timestep.
//' @param parameters List with parameters.
//' @param pop_n Integer with number of individuals.
//' @param movement String specifing movement algorithm. Either 'rand', 'attr' or 'behav'.
//' @param max_dist Double with maximum movement distance.
//' @param pop_reserves_thres Vector with threshold of pop_reserves_max to drain prior to foraging.
//' @param coords_reef Matrix with ID and coords of reef cells.
//' @param cell_adj Matrix with cell adjacencies.
//' @param extent Vector with extent (xmin,xmax,ymin,ymax).
//' @param dimensions Vector with dimensions (nrow, ncol).
//' @param nutrients_input Vector with amount of nutrient input each timestep.
//' @param max_i Integer with maximum number of simulation timesteps.
//' @param min_per_i Integer to specify minutes per i.
//' @param save_each Numeric how often data should be saved to return.
//' @param seagrass_each Integer how often (each i * x) seagrass dynamics will be simulated.
//' @param burn_in Numeric with timesteps used to burn in.
//' @param verbose If TRUE, progress reports are printed.
//'
//' @details
//' The functions is a 'wrapper' around the following sub-processes: (i) nutrient input,
//' (ii) seagrass growth, (iii) detritus mineralization, (iv) movement of individuals,
//' (v) respiration of individuals, (vi) growth of individuals, (vii) mortality of individuals,
//' (viii) diffusion of nutrients/detritus, and ix) nutrient output.
//'
//' @references
//' For a detailed model description see Esquivel, K.E., Hesselbarth, M.H.K., Allgeier, J.E.
//' Mechanistic support for increased primary production around artificial reefs. Manuscript
//' submitted for publication.
//'
//' @return void
//'
//' @aliases rcpp_sim_processes
//' @rdname rcpp_sim_processes
//'
//' @export
// [[Rcpp::export]]
void rcpp_sim_processes(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix fishpop,
                        Rcpp::List seafloor_track, Rcpp::List fishpop_track, Rcpp::List parameters,
                        int pop_n, Rcpp::String movement, double max_dist, Rcpp::NumericVector pop_reserves_thres,
                        Rcpp::NumericMatrix coords_reef, Rcpp::NumericMatrix cell_adj,
                        Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                        Rcpp::NumericVector nutrients_input,
                        int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in,
                        bool verbose) {

  // setup progress bar
  Progress progress(max_i, verbose);

  // inital flags to run processes //

  // flag if diffusion needs to be run
  bool flag_diffuse = (as<double>(parameters["nutrients_diffusion"]) > 0.0) ||
    (as<double>(parameters["detritus_diffusion"]) > 0.0) ||
    (as<double>(parameters["detritus_fish_diffusion"])) > 0.0;

  // get input flag
  bool flag_input = Rcpp::sum(nutrients_input) > 0.0;

  // flag if output needs to be run
  bool flag_output = (as<double>(parameters["nutrients_loss"]) > 0.0) ||
    (as<double>(parameters["detritus_loss"]) > 0.0);

  // init seafloor stuff //

  // get only ID of reefs as vector
  Rcpp::NumericVector cells_reef = coords_reef(_, 0);

  // init various stuff //

  // calc time_frac for rcpp_seagrass_growth
  double time_frac = (min_per_i / 60.0) * seagrass_each;

  // save original data //

  // save input data in tracking list
  seafloor_track[0] = Rcpp::clone(seafloor);

  fishpop_track[0] = Rcpp::clone(fishpop);

  // run simulation
  for (int i = 1; i <= max_i; i++) {

    // check abort of function
    if (Progress::check_abort()) {

      Rcpp::stop("Stopped by user.");

    }

    // simulate seagrass only each seagrass_each iterations
    if ((i % seagrass_each) == 0) {

      // calculate counter for nutrient input vector
      int i_temp = (i / seagrass_each) - 1;

      // simulate nutrient input if present
      if (flag_input && (nutrients_input[i_temp] > 0.0)) {

        rcpp_nutr_input(seafloor, nutrients_input[i_temp]);

      }

      // simulate seagrass growth
      rcpp_seagrass_growth(seafloor, cells_reef,
                           parameters["bg_v_max"], parameters["bg_k_m"], parameters["bg_gamma"],
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
    if ((i > burn_in) && (pop_n != 0)) {

      // calculate new coordinates and activity
      rcpp_move_wrap(fishpop, coords_reef, movement, pop_reserves_thres,
                     parameters["move_mean"], parameters["move_var"],
                     parameters["move_reef"], parameters["move_border"],
                     parameters["move_return"], max_dist, extent, dimensions);

      // simulate fish respiration (26°C is mean water temperature in the Bahamas)
      rcpp_respiration(fishpop,
                       parameters["resp_intercept"], parameters["resp_slope"],
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

    // run remaining seagrass subprocesses
    if ((i % seagrass_each) == 0) {

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

/*** R
rcpp_sim_processes(seafloor = seafloor_values, fishpop = fishpop_values,
                   seafloor_track = seafloor_track, fishpop_track = fishpop_track,
                   parameters = parameters, pop_n = starting_values$pop_n,
                   movement = movement, max_dist = max_dist, pop_reserves_thres = pop_reserves_thres,
                   coords_reef = coords_reef, extent = extent, dimensions = dimensions, nutrients_input = nutrients_input,
                   max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                   seagrass_each = seagrass_each, burn_in = burn_in, verbose = verbose)
*/
