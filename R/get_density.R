#' get_density
#'
#' @description Get density of fish occurrence within each raster cell.
#'
#' @param result mdl_rn object of simulation run.
#' @param timestep Integer to specify maximum timestep.
#' @param normalize Logical if TRUE count is divided by timesteps.
#'
#' @details
#' Calculates the fish density for each cells. This means the total count of
#' fish occurrence within a raster cell is divided by the maximum timestep.
#'
#' @return data.frame
#'
#' @examples
#' # Add example code
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

  i <- timestep

  # check if i can be divided by save_each without reminder
  if (i %% result$save_each != 0) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)
  }

  # create empty raster
  ras_density <- raster::raster(ext = result$extent, resolution = result$grain)

  if (nrow(result$fishpop > 0)) {

    # remove burn_in
    if (result$burn_in > 0) {

      result$fishpop <- result$fishpop[result$fishpop$burn_in == "no", ]

    }

    # count fish within each cell
    ras_density <- raster::rasterize(x = result$fishpop[, c("x", "y")],
                                     y = ras_density,
                                     fun = "count", background = 0)

    # convert to data frame
    ras_density <- raster::as.data.frame(ras_density, xy = TRUE)

    # rename
    names(ras_density) <- c("x", "y", "density")

    # normalize by max_i
    if (normalize) {

      ras_density$density <- ras_density$density / i

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
