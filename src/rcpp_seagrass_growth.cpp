#include <Rcpp.h>

#include "rcpp_seagrass_growth.h"
#include "rcpp_allocation_ratio.h"
#include "rcpp_nutr_uptake.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' rcpp_seagrass_growth
//'
//' @description
//' Rcpp seagrass growth.
//'
//' @param seafloor Matrix with seafloor values.
//' @param bg_v_max,bg_k_m,bg_gamma,ag_v_max,ag_k_m,ag_gamma Numeric with uptake parameters.
//' @param bg_biomass_max,bg_biomass_min,ag_biomass_max,ag_biomass_min Numerich with biomass values and parameters.
//' @param seagrass_slough,seagrass_thres,seagrass_slope,time_frac Numerich with various parameters.
//'
//' @details
//' Function to simulate processes of belowground and aboveground seagrass slough and
//' growth. All processes are simulated adapted after DeAngelis (1992).
//'
//' An amount of bg and ag slough is calculated pased on \code{seagrass_slough}.
//' The ratio is decreased the smaller the biomass gets and approximates zero for
//' the minimum capacity in a cell.
//'
//' The amount of nutrients that is taken up from the water column depend on the
//' seagrass belowground and aboveground biomass and available nutrients in each
//' cell.
//'
//' Allocation of uptake in bg or ag biomass depends on the uptake amount, the
//' amount of sloughed biomass and the corresponding capacities of bg and ag in the cells.
//' Additional growth (after slough was balanced) depends on the allocation ratio.
//'
//' Last, all cells in which either bg or ag biomass is above the maximum capacity,
//' all additionall biomass is sloughed.
//'
//' @references
//' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
//' Netherlands, Dordrecht. <https://doi.org/10.1007/978-94-011-2342-6>
//'
//' @return void
//'
//' @aliases rcpp_seagrass_growth
//' @rdname rcpp_seagrass_growth
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_seagrass_growth(Rcpp::NumericMatrix seafloor,
                          double bg_v_max, double bg_k_m, double bg_gamma,
                          double ag_v_max, double ag_k_m, double ag_gamma,
                          double bg_biomass_max, double bg_biomass_min,
                          double ag_biomass_max, double ag_biomass_min,
                          double seagrass_thres, double seagrass_slope,
                          double seagrass_slough, double time_frac) {

  // loop through all seafloor cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // check if current cell is a seagrass cell
    if (seafloor(i, 15) == 0) {
      Rcout << "(64)before_row_id = " << i << " values = " << seafloor (i, 3) << std::endl;
      // calculate detritus //
      if (seafloor(i, 4) < 0) {Rcout << seafloor(i, 4); Rcpp::stop("(66) seafloor(i, 4) is neg");}
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
      Rcout << "(97) seafloor(i, 4) = " << seafloor(i, 4) << " seafloor (i, 3) = " << seafloor(i, 3)<<  std::endl;
      // calculate total possible nutrient uptake bg
      double bg_uptake = rcpp_nutr_uptake(seafloor(i, 4), seafloor(i, 3),
                                          bg_v_max, bg_k_m, time_frac);
      Rcout << "(101)bg_uptake = " << bg_uptake << std::endl;
      if (seafloor(i, 4) < 0) { Rcpp::stop("(102) stopped before bg_uptake loss");}
      // remove bg nutrients uptake
      seafloor(i, 4) -= bg_uptake;
      if (seafloor(i, 4) < 0) { Rcout << "(105) seafloor(i, 4) after bg_uptake loss = " << seafloor(i, 4) << std::endl;}
      // track bg nutrients uptake
      seafloor(i, 12) += bg_uptake;
      if (seafloor(i, 4) < 0) { Rcpp::stop("(108) stopped after bg_uptake loss");}
      // calculate total possible nutrient uptake bg
      double ag_uptake = rcpp_nutr_uptake(seafloor(i, 4), seafloor(i, 2),
                                          ag_v_max, ag_k_m, time_frac);


      // remove ag nutrients uptake
      seafloor(i, 4) -= ag_uptake;

      // track bg nutrients uptake
      seafloor(i, 11) += ag_uptake;

      // calculate total nutrient uptake
      double total_uptake = bg_uptake + ag_uptake;
      Rcout << "(122)total_uptake = " <<  total_uptake << std::endl;

      // seagrass growth //

      // uptake not big enough to keep bg stable; bg and ag shrinks
      if (total_uptake < (bg_detritus * bg_gamma)) {

        // calculate bg growth
        double bg_growth = total_uptake / bg_gamma;
        Rcout << "(131)seafloor(i,3) before bg_growth = " << seafloor(i,3) << std::endl;
        // add bg detritus to biomass
        seafloor(i, 3) += bg_growth;
        Rcout << "(134)seafloor(i,3) after bg_growth = " << seafloor(i,3) << std::endl;
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
    //      Rcout << "(177)seafloori, 3 going into a_ratio = " << seafloor(i, 3) << std::endl;
          double bg_ratio = rcpp_allocation_ratio(seafloor(i, 3),
                                                  bg_biomass_min, bg_biomass_max,
                                                  seagrass_thres, seagrass_slope);

          // calculate bg growth
          double bg_growth = (total_uptake * bg_ratio) / bg_gamma;
  //        Rcout << "(184) bg_growth = " << bg_growth << std::endl;
          // add bg growth to biomass
          seafloor(i, 3) += bg_growth;
//          Rcout << "(187) seafloor(i,3) = " << seafloor(i, 3) << std::endl;
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
        Rcout << "(207)biomass > max" << std::endl;
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
    }
    Rcout << "(239)after row id = " << i << " biomass = " << seafloor(i, 3) <<  " nutrients " << seafloor(i, 4) << std::endl;
    if (seafloor(i, 4) < 0) { Rcpp::stop("(240) seafloor(i, 4) is neg");}
  }
}
