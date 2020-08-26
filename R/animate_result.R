#' animate_result
#'
#' @description Wrapper functions to run model
#'
#' @param result RasterBrick with environment created with \code{\link{setup_environment}}.
#' @param fill Character to specify which values of environmental data is used as fill.
#' @param ... Arguments passed on to \code{gganimate::animate}.
#'
#' @details
#' Wrapper function to run model. Executes the following sub-processes (i) ...
#' (ii) ...
#'
#' Parameters include ...
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'
#' animate_result()
#'
#' }
#'
#' @aliases animate_result
#' @rdname animate_result
#'
#' @export
animate_result <- function(result, fill = "reef", ...) {

  # check if fill argument makes sense
  if (!fill %in% names(result$environment)) {

    stop("Please select valid layer as fill argument.", call. = FALSE)

  }

  gg_result <-  ggplot2::ggplot(data = result$environment) +
    ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = !! ggplot2::sym(fill))) +
    ggplot2::geom_point(data = result$population, ggplot2::aes(x = x, y = y), col = "white") +
    gganimate::transition_time(i) +
    ggplot2::scale_fill_viridis_c(option = "A") +
    ggplot2::coord_equal() +
    ggplot2::theme_classic() +
    ggplot2::ggtitle(label = "Time step i: {as.integer(frame_time)}")

  gganimate::animate(gg_result, ...)
}
