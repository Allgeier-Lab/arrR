#' int_calc_uptake
#'
#' @description Internal function
#'
#' @param nutrients Vecto with amount of wc nutrients.
#' @param biomass Vector with biomass.
#' @param v_max Numeric with maximum uptake rate.
#' @param k_m Numeric with half-saturation rate.
#'
#' @details
#' Internal function to calculate nutrient uptake (Formula 3.7).
#'
#' @return Vector
#'
#' @aliases int_int_calc_uptake
#' @rdname int_int_calc_uptake
#'
#' @keywords internal
#'
#' @export
int_calc_uptake <- function(nutrients, biomass, v_max, k_m) {

  result <- ((v_max * nutrients) / (k_m + nutrients)) * biomass

  return(result)
}
