#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_convert_nutr
//'
//' @description Rcpp convert nutrients
//'
//' @param x Numeric with nutrient amount.
//' @param to String to specify in which unit to convert.
//'
//' @details
//' Rcpp implementation to convert nutrients between g and umol.
//'
//' @return double
//'
//' @aliases rcpp_convert_nutr
//' @rdname rcpp_convert_nutr
//'
//' @keywords export
// [[Rcpp::export]]
double rcpp_convert_nutr(double x, String to) {

  // convert to gram by multiplying factor
  if (to == "g") {

    double result = x * 18.039 / std::pow(10, 6);

    return(result);

  // convert to umol by multiplying factor
  } else if (to == "umol") {

    double result = x * std::pow(10, 6) / 18.039;

    return(result);

  // throw error if to option is not present
  } else {

    throw std::range_error("Please select either to='g' or to='umol'.");

  }
}

//' rcpp_calc_nutr_uptake
//'
//' @description Rcpp calculate nutrient uptake
//'
//' @param nutrients,biomass Numeric with nutrient and biomass amount of cell.
//' @param v_max,k_m,time_frac Numeric with parameters
//'
//' @details
//' Rcpp implementation to calculate nutrient uptake of cell
//'
//' @return double
//'
//' @aliases rcpp_calc_nutr_uptake
//' @rdname rcpp_calc_nutr_uptake
//'
//' @keywords export
// [[Rcpp::export]]
double rcpp_calc_nutr_uptake(double nutrients, double biomass,
                             double v_max, double k_m, double time_frac) {

  // convert water column nutrients to umol/l
  double nutrients_umol = rcpp_convert_nutr(nutrients, "umol");

  // 1 x 1 x m = 1 cubic m = 1000l * 3m water depth
  nutrients_umol = nutrients_umol / (1000 * 3);

  // calculate bg and ag uptake depending on nutrients and biomass
  // convert uptake parameters to correct tick scale (from per h to day)
  double uptake_umol = biomass * (((v_max * time_frac) * nutrients_umol) /
                                  (k_m + nutrients_umol));

  // convert back to g
  double uptake_g = rcpp_convert_nutr(uptake_umol, "g");

  return(uptake_g);

}

//' rcpp_calc_seagrass_growth
//'
//' @description Rcpp calc seagrass growth
//'
//' @param seafloor Matrix with seafloor values.
//' @param cells_reef Vector with id of reef cells.
//' @param bg_v_max,bg_k_m,bg_gamma,ag_v_max,ag_k_m,ag_gamma Numeric with uptake parameters.
//' @param bg_biomass_max,bg_biomass_min,ag_biomass_max,ag_biomass_min Numerich with biomass values and parameters.
//' @param detritus_ratio,seagrass_thres,seagrass_slope,time_frac Numerich with various parameters.
//'
//' @details
//' Rcpp implementation to calculate seagrass growth.
//'
//' @return void
//'
//' @aliases rcpp_calc_seagrass_growth
//' @rdname rcpp_calc_seagrass_growth
//'
//' @keywords export
// [[Rcpp::export]]
void rcpp_calc_seagrass_growth(Rcpp::NumericMatrix seafloor,
                               Rcpp::NumericVector cells_reef,
                               double bg_v_max, double bg_k_m, double bg_gamma,
                               double ag_v_max, double ag_k_m, double ag_gamma,
                               double bg_biomass_max, double bg_biomass_min,
                               double ag_biomass_max, double ag_biomass_min,
                               double seagrass_thres, double seagrass_slope,
                               double detritus_ratio,
                               double time_frac) {

  // loop through all seafloor cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // counts how often current i is present in vector with reef cells
    int is_reef = std::count(cells_reef.begin(), cells_reef.end(), i + 1);

    // check if current cell is not a reef cell, i.e. count() will be 0
    if (is_reef == 0) {

      // calculate detritus //

      // calculate bg detritus modifier
      double bg_modf = (bg_biomass_max - seafloor(i, 3)) /
        (bg_biomass_max - bg_biomass_min);

      // calculate ag detritus modifier
      double ag_modf = (ag_biomass_max - seafloor(i, 2)) /
        (ag_biomass_max - ag_biomass_min);

      // calculate detritus fraction from bg biomass
      double bg_detritus = seafloor(i, 3) * (detritus_ratio * (1 - bg_modf));

      // remove detritus from bg biomass
      seafloor(i, 3) -= bg_detritus;

      // track bg slough
      seafloor(i, 10) += bg_detritus;

      // calculate detritus fraction from ag biomass
      double ag_detritus = seafloor(i, 2) * (detritus_ratio * (1 - ag_modf));

      // remove detritus from ag biomass
      seafloor(i, 2) -= ag_detritus;

      // track ag slough
      seafloor(i, 9) += ag_detritus;

      // add nutrients to detritus pool
      seafloor(i, 5) += (bg_detritus * bg_gamma) + (ag_detritus * ag_gamma);

      // calculate uptake //

      // calculate total possible nutrient uptake bg
      double bg_uptake = rcpp_calc_nutr_uptake(seafloor(i, 4), seafloor(i, 3),
                                               bg_v_max, bg_k_m, time_frac);

      // remove bg nutrients uptake
      seafloor(i, 4) -= bg_uptake;

      // track bg nutrients uptake
      seafloor(i, 12) += bg_uptake;

      // calculate total possible nutrient uptake bg
      double ag_uptake = rcpp_calc_nutr_uptake(seafloor(i, 4), seafloor(i, 2),
                                               ag_v_max, ag_k_m, time_frac);

      // remove ag nutrients uptake
      seafloor(i, 4) -= ag_uptake;

      // track bg nutrients uptake
      seafloor(i, 11) += ag_uptake;

      // calculate total nutrient uptake
      double total_uptake = ag_uptake + bg_uptake;

      // seagrass growth //

      // calc turning point of allocation function
      double midpoint = -log(2.0) / log(seagrass_thres);

      // normalize current cell to 0 - 1
      double bg_biomass_norm = (seafloor(i, 3) - bg_biomass_min) /
        (bg_biomass_max - bg_biomass_min);

      double seagrass_modf = 1 / (1 + std::pow((std::pow(bg_biomass_norm, midpoint) /
        (1 - std::pow(bg_biomass_norm, midpoint))), -seagrass_slope));

      // below threshold and uptake large enough to keep ag stable
      if (total_uptake <= (ag_detritus * ag_gamma)) {

        // calculate bg growth
        double ag_growth = total_uptake / ag_gamma;

        // track bg biomass production
        seafloor(i, 7) += ag_detritus;

      } else if (total_uptake > (ag_detritus * ag_gamma) &
        bg_biomass_norm < seagrass_thres) {

        // add growth to biomass
        seafloor(i, 2) += ag_detritus;

        // track bg biomass production
        seafloor(i, 7) += ag_detritus;

        // update uptake_temp
        total_uptake -= ag_detritus * ag_gamma;

        // calculate bg growth
        double bg_growth = total_uptake / bg_gamma;

        // add bg growth to biomass
        seafloor(i, 3) += bg_growth;

        // track bg biomass production
        seafloor(i, 8) += bg_growth;

      // above threshold or uptake not large enough to keep ag stable
      } else {

        // calculate bg growth
        double bg_growth = (total_uptake * (1 - seagrass_modf)) / bg_gamma;

        // add bg growth to biomass
        seafloor(i, 3) += bg_growth;

        // track bg biomass production
        seafloor(i, 8) += bg_growth;

        // calculate ag growth
        double ag_growth = (total_uptake * seagrass_modf) / ag_gamma;

        // add ag growth to biomass
        seafloor(i, 2) += ag_growth;

        // track ag biomass production
        seafloor(i, 7) += ag_growth;

      }

      // check if biomass is within min/max values //

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

    // reef cell; do nothing
    } else {

      continue;

    }
  }
}

/*** R
rcpp_calc_seagrass_growth(seafloor = seafloor_values,
                          cells_reef = cells_reef,
                          bg_v_max = parameters$bg_v_max, bg_k_m = parameters$bg_k_m,
                          bg_gamma = parameters$bg_gamma,
                          ag_v_max = parameters$ag_v_max, ag_k_m = parameters$ag_k_m,
                          ag_gamma = parameters$ag_gamma,
                          bg_biomass_max = parameters$bg_biomass_max,
                          bg_biomass_min = parameters$bg_biomass_min,
                          ag_biomass_max = parameters$ag_biomass_max,
                          ag_biomass_min = parameters$ag_biomass_min,
                          detritus_ratio = parameters$detritus_ratio,
                          seagrass_thres = parameters$seagrass_thres,
                          min_per_i = min_per_i)
*/
