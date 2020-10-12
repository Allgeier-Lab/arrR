#' calc_nutr_uptake
#'
#' @description Internal function to calculate nutrient uptake
#'
#' @param nutrients Add info about parameter.
#' @param gamma Add info about parameter.
#' @param detritus_ratio Add info about parameter.
#'
#' @details
#' Add details
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
#' @aliases int_calc_nutr_uptake
#' @rdname int_calc_nutr_uptake
#'
#' @keywords internal
#'
#' @export
int_calc_nutr_uptake <- function(nutrients, bg_biomass, ag_biomass,
                                 v_max, k_m, time_fac) {

  # convert water column nutrients to umol/l
  nutrients_umol <- int_convert_nutr(nutrients,
                                     to = "umol") / 10000

  # calculate bg and ag uptake depending on nutrients and biomass
  # convert uptake parameters to correct tick scale (from per h to day)
  uptake_bg_umol <- bg_biomass * int_calc_monod(nutrients = nutrients_umol,
                                                v_max = v_max[1] * time_fac,
                                                k_m = k_m[1])

  uptake_ag_umol <- ag_biomass * int_calc_monod(nutrients = nutrients_umol,
                                                v_max = v_max[2] * time_fac,
                                                k_m = k_m[2])

  # sum bg and ag to get total uptake in g
  uptake_total_g <- int_convert_nutr(x = uptake_bg_umol + uptake_ag_umol, to = "g")

  # check if total uptake exceeds total available nutrients
  uptake_total_g <- ifelse(test = uptake_total_g > nutrients,
                           yes = nutrients, no = uptake_total_g)

  return(uptake_total_g)
}
