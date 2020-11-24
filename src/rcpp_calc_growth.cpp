#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_calc_growth
//'
//' @description Rcpp calc growth
//'
//' @param fish_pop Matrix with fishpop values.
//' @param seafloor Matrix with seafloor values.
//' @param growth_values Matrix with growth values.
//' @param pop_n_body,pop_max_reserves,min_per_i Numeric with parameters.
//'
//' @details
//' \code{Rcpp} implementation of to calculate growth.
//'
//' @return Matrix
//'
//' @name rcpp_calc_growth
//'
//' @export
// [[Rcpp::export]]
void rcpp_calc_growth(Rcpp::NumericMatrix fish_pop,
                      Rcpp::NumericMatrix seafloor,
                      Rcpp::NumericMatrix growth_values,
                      double pop_n_body, double pop_max_reserves,
                      double min_per_i) {

  // get number of fish
  int n_pop = fish_pop.nrow();

  // create vector for reserves difference
  double reserves_diff;

  for (int i = 0; i < n_pop; i++) {

    //  increase age (60 min * 24 h = 1440 min/day)
    fish_pop(i, 1) += min_per_i / 1440;

    // increase fish dimensions
    fish_pop(i, 2) += growth_values(i, 1);

    fish_pop(i, 3) += growth_values(i, 2);

    // update reserves
    fish_pop(i, 5) = fish_pop(i, 3) * pop_n_body * pop_max_reserves;

    // calculate reserves difference
    reserves_diff = fish_pop(i, 5) - fish_pop(i, 4);

    // consumption requirement can be meet by detritus_pool
    if (growth_values(i, 0) <= seafloor(i, 1)) {

      // calculate remaining nutrients in pool
      double nutrients_left = seafloor(i, 1) - growth_values(i, 0);

      // reserves can be filled completely
      if (reserves_diff <= nutrients_left) {

        // save consumption
        seafloor(i, 3) += growth_values(i, 0) + reserves_diff;

        // set reserves to max
        fish_pop(i, 4) = fish_pop(i, 5);

        // reduce detritus pool
        seafloor(i, 1) = nutrients_left - reserves_diff;

      // reserves cannot be filled completely by nutrient pool
      } else {

        // save consumption
        seafloor(i, 3) += growth_values(i, 0) + nutrients_left;

        // add all nutrients that are left
        fish_pop(i, 4) += nutrients_left;

        // set detritus pool to zero
        seafloor(i, 1) = 0;
      }

    // reserves are needed to meet consumption requirement
    } else {

      // save consumption
      seafloor(i, 3) += seafloor(i, 1);

      // reduced reserves
      fish_pop(i, 4) -= growth_values(i, 0) - seafloor(i, 1);

      // set detritus pool to zero
      seafloor(i, 1) = 0;

    }

    // calc non-used consumption (excretion)
    double excretion_temp = growth_values(i, 0) - (growth_values(i, 2) * pop_n_body);

    // save excretion
    seafloor(i, 4) += excretion_temp;

    // add non-used consumption to nutrient pool (excretion)
    seafloor(i, 0) += excretion_temp;

  }

}

/*** R
rcpp_calc_growth(fish_pop = fish_temp,
                 seafloor = seafloor_temp,
                 growth_values = growth_values,
                 pop_n_body = parameters$pop_n_body,
                 pop_max_reserves = parameters$pop_max_reserves,
                 min_per_i = min_per_i)
*/
