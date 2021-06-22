#' get_stable_values
#'
#' @description
#' Get values for stable biomass growth.
#'
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#' @param fishpop Logical if TRUE and estimate of the maximum consumption is added to detritus.
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Returns a list with starting values for i) the nutrients_pool and ii) the detritus_pool
#' which allow stable seagrass growth if no fish individuals are present. This means,
#' that both pools contain exactly the amount to balance the sloughed bg_biomass and ag_biomass
#' each  timestep. If \code{fishpop = TRUE} and estimate of detritus consumption of one fish
#' individual of maximum size is added to the detritus pool.
#'
#' @return list
#'
#' @examples
#' get_stable_values(starting_values = default_starting_values,
#' parameters = default_parameters)
#'
#' @aliases get_stable_values
#' @rdname get_stable_values
#'
#' @export
get_stable_values <- function(starting_values, parameters, fishpop = FALSE, min_per_i = NULL) {

  # calculate detritus modifier for bg biomass
  bg_modf <- (starting_values$bg_biomass - parameters$bg_biomass_min) /
    (parameters$bg_biomass_max - parameters$bg_biomass_min)

  # calculate detritus modifier for ag biomass
  ag_modf <- (starting_values$ag_biomass - parameters$ag_biomass_min) /
    (parameters$ag_biomass_max - parameters$ag_biomass_min)

  # calculate ag detritus
  bg_detritus <- starting_values$bg_biomass * (parameters$seagrass_slough * bg_modf)

  # calculate ag detritus
  ag_detritus <- starting_values$ag_biomass * (parameters$seagrass_slough * ag_modf)

  # calculate amount of nutrients to keep ag and bg stable
  nutrients_pool <- (bg_detritus * parameters$bg_gamma) +
    (ag_detritus * parameters$ag_gamma)

  # calculate detritus  amount for stable nutrients minus slough amount
  detritus_pool <- (nutrients_pool / parameters$detritus_mineralization) -
    ((ag_detritus * parameters$ag_gamma) + (bg_detritus * parameters$bg_gamma))

  # if detritus_mineralization is zero detritus pool will be Inf
  detritus_pool <- ifelse(test = is.infinite(detritus_pool),
                          yes = 0, no = detritus_pool)

  # calculate amount of consumpotion for maximum size
  if (fishpop) {

    if (is.null(min_per_i)) {

      stop("Please provide 'min_per_i' argument", call. = FALSE)

    }

    # get length of individual, set to maximum
    length <- parameters$pop_linf

    # calculate weight of maximum size
    weight <- parameters$pop_a * (length ^ parameters$pop_b)

    # calc growth in length and weight for maximum pop_linf
    growth_length <- parameters$pop_k / (365.0 * 24.0 * 60.0) * min_per_i *
      (parameters$pop_linf - length)

    growth_weight <- parameters$pop_a * ((length + growth_length) ^ parameters$pop_b -
                                           length ^ parameters$pop_b)

    # mean respiration value based on simulation_runs
    # MH: use parameters values to calc
    respiration <- 0.001

    # calculate consumption requirements
    consumption_require <- ((growth_weight + respiration * weight) / 0.55) * parameters$pop_n_body

    # add to detritus pool
    detritus_pool <- detritus_pool + consumption_require

  }

  # combine to result list
  result <- list(nutrients_pool = nutrients_pool, detritus_pool = detritus_pool)

  return(result)
}
