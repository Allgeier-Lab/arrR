#include "rcpp_seagrass_growth.h"
#include "rcpp_allocation_ratio.h"
#include "rcpp_nutr_uptake.h"

//' rcpp_seagrass_growth
//'
//' @description Rcpp seagrass growth
//'
//' @param seafloor Matrix with seafloor values.
//' @param cells_reef Vector with ID of reef cells.
//' @param bg_v_max,bg_k_m,bg_gamma,ag_v_max,ag_k_m,ag_gamma Numeric with uptake parameters.
//' @param bg_biomass_max,bg_biomass_min,ag_biomass_max,ag_biomass_min Numerich with biomass values and parameters.
//' @param seagrass_slough,seagrass_thres,seagrass_slope,time_frac Numerich with various parameters.
//'
//' @details
//' Function to simulate processes of aboveground and belowground seagrass slough and growth.
//'
//' @references
//' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
//' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
//'
//' @return void
//'
//' @aliases rcpp_seagrass_growth
//' @rdname rcpp_seagrass_growth
//'
//' @export
// [[Rcpp::export]]
void rcpp_seagrass_growth(Rcpp::NumericMatrix seafloor, Rcpp::NumericVector cells_reef,
                          double bg_v_max, double bg_k_m, double bg_gamma,
                          double ag_v_max, double ag_k_m, double ag_gamma,
                          double bg_biomass_max, double bg_biomass_min,
                          double ag_biomass_max, double ag_biomass_min,
                          double seagrass_thres, double seagrass_slope,
                          double seagrass_slough, double time_frac) {

  // loop through all seafloor cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // counts how often current i is present in vector with reef cells
    int is_reef = std::count(cells_reef.begin(), cells_reef.end(), i + 1);

    // check if current cell is not a reef cell, i.e. count() will be 0
    if (is_reef == 0) {

      // calculate detritus //

      // calculate bg detritus modifier
      double bg_modf = (seafloor(i, 3) - bg_biomass_min) /
        (bg_biomass_max - bg_biomass_min);

      // calculate detritus fraction from bg biomass
      double bg_detritus = seafloor(i, 3) * (seagrass_slough * bg_modf);

      // remove detritus from bg biomass
      seafloor(i, 3) -= bg_detritus;

      // track bg slough
      seafloor(i, 10) += bg_detritus;

      // calculate ag detritus modifier
      double ag_modf = (seafloor(i, 2) - ag_biomass_min) /
        (ag_biomass_max - ag_biomass_min);

      // calculate detritus fraction from ag biomass
      double ag_detritus = seafloor(i, 2) * (seagrass_slough * ag_modf);

      // remove detritus from ag biomass
      seafloor(i, 2) -= ag_detritus;

      // track ag slough
      seafloor(i, 9) += ag_detritus;

      // add nutrients to detritus pool
      seafloor(i, 5) += (ag_detritus * ag_gamma) + (bg_detritus * bg_gamma);

      // calculate uptake //

      // calculate total possible nutrient uptake bg
      double bg_uptake = rcpp_nutr_uptake(seafloor(i, 4), seafloor(i, 3),
                                          bg_v_max, bg_k_m, time_frac);

      // remove bg nutrients uptake
      seafloor(i, 4) -= bg_uptake;

      // track bg nutrients uptake
      seafloor(i, 12) += bg_uptake;

      // calculate total possible nutrient uptake bg
      double ag_uptake = rcpp_nutr_uptake(seafloor(i, 4), seafloor(i, 2),
                                          ag_v_max, ag_k_m, time_frac);

      // remove ag nutrients uptake
      seafloor(i, 4) -= ag_uptake;

      // track bg nutrients uptake
      seafloor(i, 11) += ag_uptake;

      // calculate total nutrient uptake
      double total_uptake = bg_uptake + ag_uptake;

      // seagrass growth //

      // uptake not big enough to keep bg stable; bg and ag shrinks
      if (total_uptake < (bg_detritus * bg_gamma)) {

        // calculate bg growth
        double bg_growth = total_uptake / bg_gamma;

        // add bg detritus to biomass
        seafloor(i, 3) += bg_growth;

        // track bg biomass production
        seafloor(i, 8) += bg_growth;

      // uptake big enough to keep bg stable
      } else {

        // add bg detritus to biomass
        seafloor(i, 3) += bg_detritus;

        // track bg biomass production
        seafloor(i, 8) += bg_detritus;

        // remove bg detritus from uptake
        total_uptake -= bg_detritus * bg_gamma;

        // remaining uptake cannot keep ag stable; ag shrinks
        if (total_uptake < (ag_detritus * ag_gamma)) {

          // calculate ag growth
          double ag_growth = total_uptake / ag_gamma;

          // add ag detritus to biomass
          seafloor(i, 2) += ag_growth;

          // track ag biomass production
          seafloor(i, 7) += ag_growth;

        // keep ag stable and use remaining nutrients according to sigmoid
        } else {

          // add ag detritus to biomass
          seafloor(i, 2) += ag_detritus;

          // track ag biomass production
          seafloor(i, 7) += ag_detritus;

          // update uptake
          total_uptake -= ag_detritus * ag_gamma;

          // additional growth //

          // calculate potential allocation ratio
          double bg_ratio = rcpp_allocation_ratio(seafloor(i, 3),
                                                  bg_biomass_min, bg_biomass_max,
                                                  seagrass_thres, seagrass_slope);

          // calculate bg growth
          double bg_growth = (total_uptake * bg_ratio) / bg_gamma;

          // add bg growth to biomass
          seafloor(i, 3) += bg_growth;

          // track bg biomass production
          seafloor(i, 8) += bg_growth;

          // calculate ag growth
          double ag_growth = (total_uptake * (1 - bg_ratio)) / ag_gamma;

          // add ag growth to biomass
          seafloor(i, 2) += ag_growth;

          // track ag biomass production
          seafloor(i, 7) += ag_growth;

        }
      }

      // check if biomass is within min/max values //

      // check belowground biomass
      if (seafloor(i, 3) > bg_biomass_max) {

        // calculate difference between current and max
        bg_detritus = seafloor(i, 3) - bg_biomass_max;

        // remove difference from biomass
        seafloor(i, 3) -= bg_detritus;

        // add nutrients of biomass to detritus pool
        seafloor(i, 5) += (bg_detritus * bg_gamma);

        // track ag slough
        seafloor(i, 10) += bg_detritus;

      }

      // check if ag biomass is above max
      if (seafloor(i, 2) > ag_biomass_max) {

        // calculate difference between current and max
        ag_detritus = seafloor(i, 2) - ag_biomass_max;

        // remove difference from biomass
        seafloor(i, 2) -= ag_detritus;

        // add nutrients of biomass to detritus pool
        seafloor(i, 5) += (ag_detritus * ag_gamma);

        // track ag slough
        seafloor(i, 9) += ag_detritus;

      }

    // No reef cell
    } else {

      continue;

    }
  }
}

/*** R
rcpp_seagrass_growth(seafloor = seafloor_values, coords_reef = coords_reef,
                     bg_v_max = parameters$bg_v_max, bg_k_m = parameters$bg_k_m, bg_gamma = parameters$bg_gamma,
                     ag_v_max = parameters$ag_v_max, ag_k_m = parameters$ag_k_m, ag_gamma = parameters$ag_gamma,
                     bg_biomass_max = parameters$bg_biomass_max, bg_biomass_min = parameters$bg_biomass_min,
                     ag_biomass_max = parameters$ag_biomass_max, ag_biomass_min = parameters$ag_biomass_min,
                     seagrass_thres = parameters$seagrass_thres, seagrass_slough = parameters$seagrass_slough,
                     min_per_i = min_per_i)
*/
