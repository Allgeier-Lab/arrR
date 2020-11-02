#' calc_nutr_uptake
#'
#' @description Internal function to calculate nutrient uptake
#'
#' @param nutrients Vector with nutrients pool values.
#' @param biomass Vector with biomass values.
#' @param v_max,k_m Parameters of Michaelis–Menten model.
#' @param time_fac Numeric that specifies time period one iteration corresponds to.
#'
#' @details
#' Calculates the nutrient uptake for a given nutrients concentration in the water
#' column and biomass. The total uptake is based on the Michaelis–Menten model which
#' describes the acutal uptake in relation to the total possible uptake based on
#' the nutrient concentration.
#'
#' @references
#' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
#' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
#'
#' Lee, K.-S., Dunton, K.H., 1999. Inorganic nitrogen acquisition in the seagrass
#' Thalassia testudinum: Development of a whole-plant nitrogen budget.
#' Limnol. Oceanogr. 44, 1204–1215. https://doi.org/10.4319/lo.1999.44.5.1204
#'
#' @return vector
#'
#' @aliases calc_nutr_uptake
#' @rdname calc_nutr_uptake
#'
#' @export
calc_nutr_uptake <- function(nutrients, biomass, v_max, k_m, time_fac) {

  # convert water column nutrients to umol/l
  nutrients_umol <- convert_nutr(nutrients, to = "umol") / 10000

  # calculate bg and ag uptake depending on nutrients and biomass
  # convert uptake parameters to correct tick scale (from per h to day)
  uptake_umol <- biomass * (((v_max * time_fac) * nutrients_umol) /
                              (k_m + nutrients_umol))

  # sum bg and ag to get total uptake in g
  uptake_g <- convert_nutr(x = uptake_umol, to = "g")

  return(uptake_g)
}
