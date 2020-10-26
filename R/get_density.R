#' get_density
#'
#' @description Get density of fish
#'
#' @param result mdl_rn object of simulation run.
#' @param timestep Integer to specify which timestep is plotted.
#' @param plot Logical if true result is plotted.
#' @param base_size Numeric to specify base font size.
#'
#' @details
#' Return raster with density of fish.
#'
#' @seealso
#' \code{\link{run_simulation}}
#'
#' @examples
#' \dontrun{
#'
#' example code
#'
#' }
#'
#' @aliases get_density
#' @rdname get_density
#'
#' @export
get_density <- function(result, timestep = result$max_i, plot = FALSE,  base_size = 10) {

  i <- timestep

  # check if i can be divided by save_each without reminder
  if (i %% result$save_each != 0) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)
  }

  # create empty raster
  ras_density <- raster::raster(ext = result$extent, resolution = result$grain)

  if (nrow(result$fish_population > 0)) {

    fish_population <- subset(result$fish_population,
                              timestep <= i, select = c("x", "y"))

    # count fish within each cell
    ras_density <- raster::rasterize(x = result$fish_population[, c("x", "y")],
                                     y = ras_density,
                                     fun = "count", background = 0)

    # convert to data frame
    ras_density <- raster::as.data.frame(ras_density, xy = TRUE)

    # rename
    names(ras_density) <- c("x", "y", "density")

    # normalize by max_i
    ras_density$density <- ras_density$density / i

  } else {

    # conver to dataframe
    ras_density <- raster::as.data.frame(ras_density, xy = TRUE)

    # set density to 0
    ras_density$layer <- 0

    # rename
    names(ras_density) <- c("x", "y", "density")


  }

  # return dataframe
  if (!plot) {

    return(ras_density)

  # return ggplot
  } else {

    # create plot
    gg_density <- ggplot2::ggplot(data = ras_density) +
      ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = density)) +
      ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                    na.value = "#9B964A", name = "Density") +
      ggplot2::coord_equal() +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::labs(title = paste0("Simulation time: ",
                                   round(i * result$min_per_i / 60 / 24, 1),
                                   " days\n(Timesteps: ", i, ")")) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    return(gg_density)

  }
}
