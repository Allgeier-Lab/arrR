#' plot.mdl_rn
#'
#' @description Plotting method for mdl_rn object
#'
#' @param x Random patterns.
#' @param fill Character to specify which values of environmental data is used as fill.
#' @param i Integer to specify which timestep is plotted.
#' @param ... Not used.
#'
#' @details
#' Plotting method for model run results simulated with \code{\link{run_simulation}}.
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
#' @aliases plot.mdl_rn
#' @rdname plot.mdl_rn
#'
#' @export
plot.mdl_rn <- function(x, fill = "reef", i = x$max_i, ...) {

  # get seafloor values of last timestep
  seafloor <- subset(x$seafloor, timestep == i, select = -timestep)

  # get fish population values of last timestep
  fish_population <- subset(x$fish_population, timestep == i, select = -timestep)

  # use discrete scale
  if (fill == "reef") {

    seafloor$reef <- factor(seafloor$reef, levels = c(0, 1),
                            labels = c("Seafloor", "Artifical reef"))

    # create plot
    gg_result <- ggplot2::ggplot(data = seafloor) +
      ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = reef)) +
      ggplot2::geom_point(data = fish_population, ggplot2::aes(x = x, y = y), shape = 1) +
      ggplot2::scale_fill_manual(values = c("#E9EAF0", "#9B964A"), name = "Cover Type") +
      ggplot2::coord_equal() +
      ggplot2::theme_classic() +
      ggplot2::labs(title = paste0("Timestep: ", i,
                                   "; Simulation time: ", round(i * x$min_per_i / 60 / 24, 1), " days"))

    # use continuous scale
  } else if (fill %in% c("ag_biomass", "bg_biomass", "detritus_pool",
                         "detritus_dead", "wc_nutrients")) {

    # reclassify AR as NA for better plotting
    seafloor[seafloor$reef == 1, fill] <- NA

    # create plot
    gg_result <- ggplot2::ggplot(data = seafloor) +
      ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = !! ggplot2::sym(fill))) +
      ggplot2::geom_point(data = fish_population, ggplot2::aes(x = x, y = y), shape = 1) +
      ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                    na.value = "#9B964A") +
      ggplot2::coord_equal() +
      ggplot2::theme_classic() +
      ggplot2::labs(title = paste0("Timestep: ", i,
                                   "; Simulation time: ", round(i * x$min_per_i / 60 / 24, 1), " days"))

  } else if (fill == "density") {

    fish_population <- subset(x$fish_population, timestep <= i, select = -timestep)

    ras_density <- raster::raster(ext = x$extent, resolution = x$grain)

    ras_density <- raster::rasterize(x = x$fish_population[, c("x", "y")],
                                     y = ras_density,
                                     fun = "count", background = 0)

    ras_density <- raster::as.data.frame(ras_density, xy = TRUE)

    ras_density$layer <- ras_density$layer / i

    # reclassify AR as NA for better plotting
    ras_density[seafloor$reef == 1, "layer"] <- NA

    # create plot
    gg_result <- ggplot2::ggplot(data = ras_density) +
      ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = layer)) +
      ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                    na.value = "#9B964A", name = "Density") +
      ggplot2::coord_equal() +
      ggplot2::theme_classic() +
      ggplot2::labs(title = paste0("Timestep: ", i,
                                   "; Simulation time: ", round(i * x$min_per_i / 60 / 24, 1), " days"))


  # check if fill argument makes sense
  } else {

    stop("Please select a valid layer as 'fill' argument.", call. = FALSE)

  }

  return(gg_result)

}
