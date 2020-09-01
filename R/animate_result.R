#' animate_result
#'
#' @description Wrapper functions to animate results
#'
#' @param result RasterBrick with environment created with \code{\link{setup_seafloor}}.
#' @param fill Character to specify which values of environmental data is used as fill.
#' @param ... Arguments passed on to \code{gganimate::animate}.
#'
#' @details
#' Animate results
#'
#' @return data frame
#'
#' @aliases animate_result
#' @rdname animate_result
#'
#' @export
animate_result <- function(result, fill = "reef", ...) {

  # use discrete scale
  if (fill == "reef") {

    fill_manual <- ggplot2::scale_fill_manual(values = c("#E9EAF0", "#9B964A"),
                                              name = "Cover Type")

    result$seafloor$reef <- factor(result$seafloor$reef, levels = c(0, 1),
                                   labels = c("Seafloor", "Artifical reef"))

  # use continuous scale
  } else if (fill %in% c("ag_biomass", "bg_biomass", "detritus_pool",
                  "detritus_dead", "wc_nutrients")) {

    # specifiy fill values; AR cells will be classified as NA
    fill_manual <- ggplot2::scale_fill_gradientn(colours = c("#E9EAF0",
                                                             "#368AC0",
                                                             "#EC747F"),
                                                 na.value = "#9B964A")

    # reclassify AR as NA for better plotting
    result$seafloor[result$seafloor$reef == 1, fill] <- NA

  # check if fill argument makes sense
  } else {

    stop("Please select a valid layer as 'fill' argument.", call. = FALSE)

  }

  # create plot
  gg_result <- ggplot2::ggplot(data = result$seafloor) +
    ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = !! ggplot2::sym(fill))) +
    ggplot2::geom_point(data = result$fish_population,
                        ggplot2::aes(x = x, y = y), shape = 1, col = "black") +
    gganimate::transition_time(track_i) +
    fill_manual +
    ggplot2::coord_equal() +
    ggplot2::theme_classic() +
    ggplot2::ggtitle(label = "Time step i: {as.integer(frame_time)}")

  gganimate::animate(gg_result, ...)
}
