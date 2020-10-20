#' calc_foo
#'
#' @description Calculate sigmoid to distribute uptake.
#'
#' @param x Seafloor_values or fish_population.
#' @param foo Summary function.
#' @param what Character.
#'
#' @details
#' Calculates foo summary function of object cols.
#'
#' @return vector
#'
#' @aliases int_int_calc_foo
#' @rdname int_int_calc_foo
#'
#' @keywords internal
#'
#' @export
int_calc_foo <- function(x, foo, what) {

  if (what == "seafloor") {

    result <- c(ag_biomass = foo(x$ag_biomass, na.rm = TRUE),
                bg_biomass = foo(x$bg_biomass, na.rm = TRUE),
                nutrients_pool = foo(x$nutrients_pool, na.rm = TRUE),
                detritus_pool = foo(x$detritus_pool, na.rm = TRUE),
                detritus_dead = foo(x$detritus_dead, na.rm = TRUE))

  } else if (what == "fish_population") {

    result <- c(length = foo(x$length, na.rm = TRUE),
                weight = foo(x$weight, na.rm = TRUE),
                reserves = foo(x$reserves, na.rm = TRUE),
                reserves_max = foo(x$reserves_max, na.rm = TRUE),
                died_consumption = foo(x$died_consumption, na.rm = TRUE),
                died_background = foo(x$died_background, na.rm = TRUE))

  }

  else {

    stop("Please select valid 'what' argument.", call. = FALSE)

  }

  return(result)
}
