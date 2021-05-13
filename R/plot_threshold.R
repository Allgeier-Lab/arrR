#' plot_threshold
#'
#' @description Plotting method for cells above threshold
#'
#' @param mdl_rn \code{mdl_rn} object of \code{\{link{simulation run}}.
#' @param timestep Integer with timestep to plot. If \code{NULL}, all timesteps are plotted.
#' @param base_size Numeric to specify base font size.
#'
#' @details.
#' Plots all belowground cells in which biomass is above \code{seagrass_thres}
#'
#' @examples
#' # Add example code
#'
#' @aliases plot_threshold
#' @rdname plot_threshold
#'
#' @export
plot_threshold <- function(mdl_rn, timestep = NULL, base_size = 10) {

  if (is.null(timestep)) {

    # filter no reef cells and needed cols
    seafloor <- mdl_rn$seafloor[mdl_rn$seafloor$reef == 0,
                                c("timestep", "bg_biomass")]

    # normalize bg biomass
    seafloor$bg_biomass <- (seafloor$bg_biomass - mdl_rn$parameters$bg_biomass_min) /
      (mdl_rn$parameters$bg_biomass_max - mdl_rn$parameters$bg_biomass_min)

    # check if above or below thres
    seafloor$thres <- ifelse(test = seafloor$bg_biomass < mdl_rn$parameters$seagrass_thres,
                             yes = "no", no = "yes")

    # convert to factor
    seafloor$thres <- factor(seafloor$thres, levels = c("yes", "no"))

    seafloor <- stats::aggregate(x = seafloor$thres,
                                 by = list(timestep = seafloor$timestep),
                                 FUN = function(x) sum(x == "yes"))

    n_cells <- nrow(mdl_rn$seafloor[mdl_rn$seafloor$timestep == 0, ]) - nrow(mdl_rn$coords_reef)

    seafloor$x <- seafloor$x / n_cells * 100

    # create title
    plot_title <- paste0("Total time : ", mdl_rn$max_i, " iterations (",
                         round(mdl_rn$max_i * mdl_rn$min_per_i / 60 / 24, 1), " days)")

    # create figure
    gg <- ggplot2::ggplot(data = seafloor) +
      ggplot2::geom_line(ggplot2::aes(x = timestep, y = x)) +
      ggplot2::scale_y_continuous(limits = c(0, 100)) +
      ggplot2::labs(x = "Timestep", y = "bg_biomass above seagrass_thres [%]",
                    title = plot_title) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                     legend.position = "bottom")

  } else {

    if (!timestep %in% mdl_rn$seafloor$timestep) {

      stop("Please select timestep that is available in data.", call. = FALSE)

    }

    # rename timestep to filter dataframe
    timestep_i <- timestep

    # get only timestep, no reef cells and needed cols
    seafloor <- mdl_rn$seafloor[mdl_rn$seafloor$timestep == timestep_i &
                                  mdl_rn$seafloor$reef == 0, c("x", "y", "bg_biomass")]

    # normalize bg biomass
    seafloor$bg_biomass <- (seafloor$bg_biomass - mdl_rn$parameters$bg_biomass_min) /
      (mdl_rn$parameters$bg_biomass_max - mdl_rn$parameters$bg_biomass_min)

    # check if above or below thres
    seafloor$thres <- ifelse(test = seafloor$bg_biomass < mdl_rn$parameters$seagrass_thres,
                             yes = "no", no = "yes")

    # convert to factor
    seafloor$thres <- factor(seafloor$thres, levels = c("yes", "no"))

    # count how manzy are above
    n_above <- length(which(seafloor$thres == "yes"))

    # create title
    plot_title <- paste0("Timestep                     : ", timestep_i, " (",
                         round(timestep_i * mdl_rn$min_per_i / 60 / 24, 1), " days)",
                         "\nCells above threshold : ", n_above)

    # create figure
    gg <- ggplot2::ggplot(data = seafloor) +
      ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = thres)) +
      ggplot2::geom_raster(data = data.frame(mdl_rn$coords_reef),
                           ggplot2::aes(x = x, y = y), fill = "#9B964A") +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_manual(name = "bg_biomass above seagrass_thres",
                                 values =  c("#EC747F", "#368AC0"), drop = FALSE) +
      ggplot2::labs(x = "", y = "", title = plot_title) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                     legend.position = "bottom")
  }

  return(gg)
}
