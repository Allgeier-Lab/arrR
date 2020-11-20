#' check_max_biomass
#'
#' @description Check if biomass is above maximum.
#'
#' @param seafloor_values Data.frame with seafloor values.
#' @param biomass_max Vector with maximum biomass.
#' @param gamma Vector with conversion factor between biomass and nutrients.
#'
#' @details
#' Checks if biomass is above maximum biomass and if so, adds difference to detritus pool.
#' For biomass_max and gamma the first element of the vectors must be the bg value
#' and the second element the ag value.
#'
#' @return data.frame
#'
#' @aliases check_max_biomass
#' @rdname check_max_biomass
#'
#' @export
check_max_biomass <- function(seafloor_values, biomass_max, gamma) {

  # check if bg biomass is above max
  bg_id <- which(seafloor_values$bg_biomass > biomass_max[1])

  # check if ag biomass is above max
  ag_id <- which(seafloor_values$ag_biomass > biomass_max[2])

  # set biomass to max biomass and add directly to detritus
  if (length(bg_id) > 0) {

    # calculate difference between current and max
    bg_diff <- seafloor_values$bg_biomass[bg_id] - biomass_max[1]

    # remove difference from biomass
    seafloor_values$bg_biomass[bg_id] <- seafloor_values$bg_biomass[bg_id] - bg_diff

    # add nutrients of biomass to detritus pool
    seafloor_values$detritus_pool[bg_id] <- seafloor_values$detritus_pool[bg_id] +
      (bg_diff * gamma[1])

  }

  # set biomass to max biomass and add directly to detritus
  if (length(ag_id) > 0) {

    # calculate difference between current and max
    ag_diff <- seafloor_values$ag_biomass[ag_id] - biomass_max[2]

    # remove difference from biomass
    seafloor_values$ag_biomass[ag_id] <- seafloor_values$ag_biomass[ag_id] - ag_diff

    # add nutrients of biomass to detritus pool
    seafloor_values$detritus_pool[ag_id] <- seafloor_values$detritus_pool[ag_id] +
      (ag_diff * gamma[2])

  }

  return(seafloor_values)
}
