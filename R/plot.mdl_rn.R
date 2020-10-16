#' plot.mdl_rn
#'
#' @description Plotting method for mdl_rn object
#'
#' @param x Random patterns.
#' @param fill Character to specify which values of environmental data is used as fill.
#' @param i Integer to specify which timestep is plotted.
#' @param base_size Numeric to specify base font size.
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
plot.mdl_rn <- function(x, fill = "reef", i = x$max_i, base_size = 10, ...) {

  # no plotting if return_mean = TRUE
  if (!is.null(x$use_summary)) {

    if (fill %in% c("ag_biomass", "bg_biomass", "nutrients_pool",
                    "detritus_pool", "detritus_dead") & length(fill) == 1) {

      # select cols
      select <- c("timestep", fill)

      # get seafloor values of last timestep
      seafloor <- subset(x$seafloor, select = select)

      gg_result <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = !! ggplot2::sym(fill))) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::labs(title = paste0("Simulation time: ",
                                     round(i * x$min_per_i / 60 / 24, 1), " days"),
                      subtitle = paste0("Timestep: ", min(seafloor$timestep), " - ", max(seafloor$timestep)))

    # fill not available
    } else {

      stop("Please select a valid layer as 'fill' argument.", call. = FALSE)

    }

  } else {

    # get seafloor values of last timestep
    seafloor <- subset(x$seafloor, timestep == i, select = -timestep)

    # use discrete scale
    if (fill == "reef") {

      seafloor$reef <- factor(seafloor$reef, levels = c(0, 1),
                              labels = c("Seafloor", "Artifical reef"))

      # create plot
      gg_result <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = reef)) +
        ggplot2::scale_fill_manual(values = c("#E9EAF0", "#9B964A"), name = "Cover Type") +
        ggplot2::coord_equal() +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::labs(title = paste0("Simulation time: ", round(i * x$min_per_i / 60 / 24, 1), " days"),
                      subtitle = paste0("Timestep: ", i))

      # use continuous scale
    } else if (fill %in% c("ag_biomass", "bg_biomass", "nutrients_pool",
                           "detritus_pool", "detritus_dead")) {

      # # reclassify AR as NA for better plotting
      # seafloor[seafloor$reef == 1, fill] <- NA

      # create plot
      gg_result <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = !! ggplot2::sym(fill))) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A") +
        ggplot2::coord_equal() +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::labs(title = paste0("Simulation time: ", round(i * x$min_per_i / 60 / 24, 1), " days"),
                      subtitle = paste0("Timestep: ", i))

    } else if (fill == "density") {

      # get fish population values of last timestep
      fish_population <- subset(x$fish_population, timestep == i, select = -timestep)

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
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::labs(title = paste0("Simulation time: ", round(i * x$min_per_i / 60 / 24, 1), " days"),
                      subtitle = paste0("Timestep: ", i))


    # check if fill argument makes sense
    } else {

      stop("Please select a valid layer as 'fill' argument.", call. = FALSE)

    }
  }

  return(gg_result)

}
