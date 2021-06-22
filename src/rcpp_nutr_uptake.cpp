#include "rcpp_nutr_uptake.h"
#include "rcpp_convert_nutr.h"

//' rcpp_nutr_uptake
//'
//' @description
//' Rcpp nutrient uptake.
//'
//' @param nutrients,biomass Numeric with nutrient and biomass amount of cell.
//' @param v_max,k_m,time_frac Numeric with parameters
//'
//' @details
//' Calculate nutrient uptake of each cells depending on avaiable nutrients in the
//' water column and biomass. All values are scaled to the time period which can be
//' specified by \code{time_frac}. Are uptaken nutrients are removed from the pool.
//' If the calculated uptake exceeds the available amount, only the  available amount
//' is taken up.
//'
//' @references
//' Lee, K.-S., Dunton, K.H., 1999. Inorganic nitrogen acquisition in the seagrass
//' Thalassia testudinum: Development of a whole-plant nitrogen budget.
//' Limnol. Oceanogr. 44, 1204–1215.https://doi.org/10.4319/lo.1999.44.5.1204
//'
//' @return double
//'
//' @aliases rcpp_nutr_uptake
//' @rdname rcpp_nutr_uptake
//'
//' @export
// [[Rcpp::export]]
double rcpp_nutr_uptake(double nutrients, double biomass,
                        double v_max, double k_m, double time_frac) {

  // convert water column nutrients to umol/l
  double nutrients_umol = rcpp_convert_nutr(nutrients, "umol");

  // calculate bg and ag uptake depending on nutrients and biomass
  double v_amb = v_max * nutrients_umol / (k_m + nutrients_umol);

  // daily uptake
  double uptake_umol = v_amb * biomass * time_frac;

  // convert back to g
  double uptake_g = rcpp_convert_nutr(uptake_umol, "g");

  // check if update was bigger than whats available
  if (uptake_g > nutrients) {

    uptake_g = nutrients;

  }

  return(uptake_g);

}

/*** R
nutrients <- seq(from = 0.0, to = 0.01, length.out = 100)

uptake <- vapply(nutrients, FUN = function(x)
  rcpp_nutr_uptake(nutrients = x, biomass = 1,
                   v_max = parameters$ag_v_max, k_m = parameters$ag_k_m, time_frac = 1),
  FUN.VALUE = numeric(1))

nutrients_umol <- vapply(nutrients, FUN = function(x) rcpp_convert_nutr(x = x, to = "umol"),
                         FUN.VALUE = numeric(1))

uptake_umol <- vapply(uptake, FUN = function(x) rcpp_convert_nutr(x = x, to = "umol"),
                      FUN.VALUE = numeric(1))

plot(x = nutrients_umol, y = uptake_umol, type = "l",
     ylim = c(0,  parameters$ag_v_max), xlim = c(0, max(nutrients_umol)))

abline(a = parameters$ag_v_max, b = 0, lty = 2, col = "grey")
*/
