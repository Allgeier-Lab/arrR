#' get_stable_values
#'
#' @description
#' Get values for stable biomass growth.
#'
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#' @param fishpop Logical if TRUE and estimate of the maximum consumption is added to detritus.
#' @param min_per_i Integer to specify minutes per i.
#' @param verbose Logical if message should be printed.
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
#' get_stable_values(starting_values = arrR_starting_values,
#' parameters = arrR_parameters)
#'
#' @aliases get_stable_values
#' @rdname get_stable_values
#'
#' @export
get_stable_values <- function(starting_values, parameters, fishpop = FALSE, min_per_i = NULL,
                              verbose = TRUE) {

  # create input flag
  flag_input <- ifelse(test = parameters$nutrients_output > 0.0,
                       yes = TRUE, no = FALSE)

  # print message about nutrient input/output needed
  if (verbose) {

    if (flag_input) {

     message("> Returning nutrient input value because nutrients_output > 0.")

    } else {

      message("> Returning no nutrient input value because nutrients_output = 0.")

    }
  }

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

  # calc output amount and set as input
  nutr_input <- nutrients_pool * parameters$nutrients_output

  # remove output amount from stable nutrients pool
  nutrients_pool <- nutrients_pool - nutr_input

  # calculate detritus  amount for stable nutrients minus slough amount
  detritus_pool <- ((nutrients_pool + nutr_input) / parameters$detritus_mineralization) -
    ((bg_detritus * parameters$bg_gamma) + (ag_detritus * parameters$ag_gamma))

  # if detritus_mineralization is zero detritus pool will be Inf
  detritus_pool <- ifelse(test = is.infinite(detritus_pool),
                          yes = 0, no = detritus_pool)

  # if input is 0, return NULL
  if (!flag_input) {

    nutr_input <- NULL

  }

  # calculate amount of consumption for maximum size
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
  result <- list(nutrients_pool = nutrients_pool, detritus_pool = detritus_pool,
                 nutr_input = nutr_input)

  return(result)
}
