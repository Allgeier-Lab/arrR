#' calc_monod
#'
#' @description Internal function
#'
#' @param nutrients Vector with amount of nutrients.
#' @param v_max Numeric with maximum uptake rate.
#' @param k_m Numeric with half-saturation rate.
#'
#' @details
#' Internal function to calculate nutrient uptake (Formula 3.7).
#'
#' @references
#' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
#' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
#'
#' Lee, K.-S., Dunton, K.H., 1999. Inorganic nitrogen acquisition in the seagrass
#' Thalassia testudinum: Development of a whole-plant nitrogen budget.
#' Limnol. Oceanogr. 44, 1204–1215. https://doi.org/10.4319/lo.1999.44.5.1204
#'
#' @return Vector
#'
#' @aliases int_calc_monod
#' @rdname int_calc_monod
#'
#' @keywords internal
#'
#' @export
int_calc_monod <- function(nutrients, v_max, k_m) {

  uptake <- ((v_max * nutrients) / (k_m + nutrients))

  return(uptake)
}
