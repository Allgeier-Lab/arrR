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

    result <- c(ag_biomass = foo(x$ag_biomass),
                bg_biomass = foo(x$bg_biomass),
                nutrients_pool = foo(x$nutrients_pool),
                detritus_pool = foo(x$detritus_pool),
                detritus_dead = foo(x$detritus_dead))

  } else if (what == "fish_population") {

    result <- c(length = foo(x$length),
                weight = foo(x$weight),
                reserves = foo(x$reserves),
                reserves_max = foo(x$reserves_max),
                died_consumption = foo(x$died_consumption),
                died_background = foo(x$died_background))

  }

  else {

    stop("Please select valid 'what'.", call. = FALSE)

  }

  return(result)
}
