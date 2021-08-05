#' get_density
#'
#' @description
#' Get density of fish individuals within cell.
#'
#' @param result mdl_rn object of simulation run.
#' @param timestep Integer to specify maximum timestep.
#' @param normalize Logical if TRUE count is divided by timesteps.
#'
#' @details
#' Calculates the fish density for each cells. Thus, the total count of
#' fish occurrences within a raster cell is divided by the maximum timestep. Please
#' keep in mind that if not each timestep was saved during \code{\link{run_simulation}},
#' the returned density might not be the "true" density because some occurrences might be missed.
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' get_density(result = result_rand)
#' }
#'
#' @aliases get_density
#' @rdname get_density
#'
#' @export
get_density <- function(result, timestep = result$max_i, normalize = FALSE) {

  # check if mdl_rn is provided
  if (!inherits(x = result, what = "mdl_rn")) {

    stop("Please prove mdl_rn object createt with run_simulation.", call. = FALSE)

  }

  timestep_slctd <- timestep

  # check if i can be divided by save_each without reminder
  if (timestep_slctd %% result$save_each != 0) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)

  }

  # return warning if save_each != 1 because not all occurences are counted
  if (result$save_each != 1) {

    warning("Please be aware that 'true' density might be higher because 'save_each' is not one.",
            call. = FALSE)

  }

  # create empty raster
  ras_density <- raster::raster(ext = result$extent, resolution = result$grain)

  if (nrow(result$fishpop > 0)) {

    # get fishpop and filter all timesteps <= than selected timestep
    fishpop_temp <- subset(result$fishpop, timestep <= timestep_slctd)

    # remove burn_in
    if (result$burn_in > 0) {

      fishpop_temp <- subset(fishpop_temp, burn_in == "no")

    }

    # count fish within each cell
    ras_density <- raster::rasterize(x = fishpop_temp[, c("x", "y")],
                                     y = ras_density,
                                     fun = "count", background = 0)

    # convert to data frame
    ras_density <- raster::as.data.frame(ras_density, xy = TRUE)

    # rename
    names(ras_density) <- c("x", "y", "density")

    # normalize by max_i
    if (normalize) {

      ras_density$density <- ras_density$density / timestep_slctd

    }

  } else {

    # convert to dataframe
    ras_density <- raster::as.data.frame(ras_density, xy = TRUE)

    # set density to 0
    ras_density$layer <- 0

    # rename
    names(ras_density) <- c("x", "y", "density")

  }

  return(ras_density)
}
