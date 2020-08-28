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

  # check if fill argument makes sense
  if (!fill %in% names(result$seafloor)) {

    stop("Please select valid layer as fill argument.", call. = FALSE)

  }

  # add layer with reef on top to control colour of reef cells result$seafloor$reef

  gg_result <-  ggplot2::ggplot(data = result$seafloor) +
    ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = !! ggplot2::sym(fill))) +
    ggplot2::geom_point(data = result$fish_population, ggplot2::aes(x = x, y = y), col = "grey") +
    gganimate::transition_time(track_i) +
    ggplot2::scale_fill_viridis_c(option = "A") +
    ggplot2::coord_equal() +
    ggplot2::theme_classic() +
    ggplot2::ggtitle(label = "Time step i: {as.integer(frame_time)}")

  gganimate::animate(gg_result, ...)
}
