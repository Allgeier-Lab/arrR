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

//' rcpp_check_max_biomass
//'
//' @description Rcpp check max biomass
//'
//' @param bg_biomass,ag_biomass,detritus_pool Numeric with values of cel..
//' @param bg_biomass_max,ag_biomass_max Parameters with maximum values.
//'
//' @details
//' Rcpp implementation to check if current biomass values are above its max. Returns
//' vector with (1) bg biomass (2) ag biomass (3) detritus pool.
//'
//' @return vector
//'
//' @aliases rcpp_check_max_biomass
//' @rdname rcpp_check_max_biomass
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_check_max_biomass(double bg_biomass, double ag_biomass,
                                           double detritus_pool,
                                           double bg_biomass_max, double ag_biomass_max) {

  // init vector for results
  Rcpp::NumericVector result (3);

  // check belowground biomass
  if (bg_biomass > bg_biomass_max) {

    // calculate difference between current and max
    double bg_diff = bg_biomass - bg_biomass_max;

    // remove difference from biomass
    bg_biomass -= bg_diff;

    // add nutrients of biomass to detritus pool
    detritus_pool += (bg_diff * 0.0082);

  }

  // check belowground biomass
  if (ag_biomass > ag_biomass_max) {

    // calculate difference between current and max
    double ag_diff = ag_biomass - ag_biomass_max;

    // remove difference from biomass
    ag_biomass -= ag_diff;

    // add nutrients of biomass to detritus pool
    detritus_pool += (ag_diff * 0.0144);

  }

  // save results to vector
  result(0) = bg_biomass;

  result(1) = ag_biomass;

  result(2) = detritus_pool;

  return(result);
}

//' rcpp_calc_seagrass_growth
//'
//' @description Rcpp calc seagrass growth
//'
//' @param seafloor Matrix with seafloor values.
//' @param cells_reef Vector with id of reef cells.
//' @param bg_v_max,bg_k_m,ag_v_max,ag_k_m Numeric with uptake parameters.
//' @param bg_biomass_max,bg_biomass_min,ag_biomass_max,ag_biomass_min Numerich with biomass values and parameters.
//' @param detritus_ratio,bg_thres,min_per_i Numerich with various parameters.
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
                               double bg_v_max, double bg_k_m,
                               double ag_v_max, double ag_k_m,
                               double bg_biomass_max, double bg_biomass_min,
                               double ag_biomass_max, double ag_biomass_min,
                               double detritus_ratio, double bg_thres,
                               double min_per_i) {

  // convert min per i to one hour
  min_per_i = min_per_i / 60;

  // loop through all seafloor cells
  for (int i = 0; i < seafloor.nrow(); i++) {

    // counts how often current i is present in vector with reef cells
    int is_reef = std::count(cells_reef.begin(), cells_reef.end(), i + 1);

    // check if current cell is not a reef cell, i.e. count() will be 0
    if (is_reef == 0) {

      // extract all values of current cell
      double ag_biomass_temp = seafloor(i, 2);

      double bg_biomass_temp = seafloor(i, 3);

      double nutrients_temp = seafloor(i, 4);

      double detritus_pool_temp = seafloor(i, 5);

      double slough_temp = seafloor(i, 7);

      // calculate total possible nutrient uptake bg
      double bg_uptake = rcpp_calc_nutr_uptake(nutrients_temp, bg_biomass_temp,
                                               bg_v_max, bg_k_m, min_per_i);

      // remove bg nutrients uptake
      nutrients_temp -= bg_uptake;

      // calculate total possible nutrient uptake bg
      double ag_uptake = rcpp_calc_nutr_uptake(nutrients_temp, ag_biomass_temp,
                                               ag_v_max, ag_k_m, min_per_i);

      // remove ag nutrients uptake
      nutrients_temp -= ag_uptake;

      // calculate total nutrient uptake
      double total_uptake_g = bg_uptake + ag_uptake;

      // calculate bg detritus modifier
      double bg_modf = (bg_biomass_max - bg_biomass_temp) /
        (bg_biomass_max - bg_biomass_min);

      // calculate ag detritus modifier
      double ag_modf = (ag_biomass_max - ag_biomass_temp) /
        (ag_biomass_max - ag_biomass_min);

      // calculate detritus fraction from bg biomass
      double bg_detritus = bg_biomass_temp * (detritus_ratio * (1 - bg_modf));

      // remove detritus from bg biomass
      bg_biomass_temp -= bg_detritus;

      // calculate detritus fraction from ag biomass
      double ag_detritus = ag_biomass_temp * (detritus_ratio * (1 - ag_modf));

      // remove detritus from ag biomass
      ag_biomass_temp -= ag_detritus;

      // calculate nutrients of total detritus
      double total_detritus = (bg_detritus * 0.0082) + (ag_detritus * 0.0144);

      // save slough amount nutrients
      slough_temp += total_detritus;

      // add nutrients to detritus pool
      detritus_pool_temp += total_detritus;

      // # i) bg biomass below threshold, but uptake not enough to keep bg/ag stable
      if ((bg_modf > (1 - bg_thres)) & (total_uptake_g <= total_detritus)) {

        bg_biomass_temp += total_uptake_g / 0.0082;

      //ii) bg biomass above threshold, but uptake not enough to keep bg stable
      } else if ((bg_modf <= (1 - bg_thres)) & (total_uptake_g <= (bg_detritus * 0.0082))) {

        bg_biomass_temp += total_uptake_g / 0.0082;

      // iii) bg biomass below threshold and uptake large enough to keep bg/ag stable
      } else if ((bg_modf > (1 - bg_thres)) & (total_uptake_g > total_detritus)) {

        // add detritus fraction for stable ag biomass
        ag_biomass_temp += ag_detritus;

        // calculate remaining nutrients
        double uptake_temp = total_uptake_g - (ag_detritus * 0.0144);

        // growth of bg biomass with remaining uptake
        bg_biomass_temp += uptake_temp / 0.0082;

      // iv) bg biomass above threshold and uptake large enough to keep bg stable
      } else if ((bg_modf <= (1 - bg_thres)) & (total_uptake_g > (bg_detritus * 0.0082))) {

        // add detritus fraction for stable bg biomass
        bg_biomass_temp += bg_detritus;

        // calculate remaining nutrients
        double uptake_temp = total_uptake_g - (bg_detritus * 0.0082);

        // calculate bg growth with remaining nutrients
        bg_biomass_temp += (uptake_temp * bg_modf) / 0.0082;

        // calculate ag growth with remaining nutrients
        ag_biomass_temp += (uptake_temp * (1 - bg_modf)) / 0.0144;

      // throw error if no case is true
      } else {

        Rcpp::stop("Error: No allocation rule was TRUE for cell %i.", i + 1);

      }

      // check maximum values
      Rcpp::NumericVector seafloor_checked = rcpp_check_max_biomass(bg_biomass_temp,
                                                                    ag_biomass_temp,
                                                                    detritus_pool_temp,
                                                                    bg_biomass_max, ag_biomass_max);

      // update seafloor values of cell
      seafloor(i, 2) =  seafloor_checked(1);

      seafloor(i, 3) = seafloor_checked(0);

      seafloor(i, 4) = nutrients_temp;

      seafloor(i, 5) = seafloor_checked(2);

      seafloor(i, 7) = slough_temp;

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
                          ag_v_max = parameters$ag_v_max, ag_k_m = parameters$ag_k_m,
                          bg_biomass_max = parameters$bg_biomass_max, bg_biomass_min = parameters$bg_biomass_min,
                          ag_biomass_max = parameters$ag_biomass_max, ag_biomass_min = parameters$ag_biomass_min,
                          detritus_ratio = parameters$detritus_ratio, bg_thres = parameters$bg_thres,
                          min_per_i = min_per_i)
*/
