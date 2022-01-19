#' plot.mdl_rn
#'
#' @description
#' Plotting method for \code{mdl_rn} object.
#'
#' @param x mdl_rn object of simulation run.
#' @param what Character specifying what to plot.
#' @param summarize Character to specify which values of environmental data is used as fill.
#' @param limits Named list with vectors with min and maximum value of values.
#' @param burn_in If TRUE, line to indicate burn-in time is plotted.
#' @param normalize Logical if TRUE count is divided by timesteps.
#' @param base_size Numeric to specify base font size.
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Not used.
#'
#' @details
#' Plotting method for model run results simulated with \code{\link{run_simulation}}.
#' The \code{what} arguments allows to either plot the seafloor values or fishpop values.
#' For both, it is possible to either plot a spatial raster at a certain timestep
#' \code{summarize = FALSE} or a summary of the values over all saved  timestep \code{summarize = TRUE}.
#' If a spatial raster is plotted, the limits can be set identical. For more information see
#' \code{\link{get_limits}}.
#'
#' @examples
#' \dontrun{
#' plot(result_rand)
#' }
#'
#' @aliases plot.mdl_rn
#' @rdname plot.mdl_rn
#'
#' @export
plot.mdl_rn <- function(x, what = "seafloor", summarize = FALSE, limits = NULL, burn_in = TRUE,
                        normalize = FALSE, base_size = 10, verbose = TRUE, ...) {

  # check if fishpop is present
  if (what == "fishpop" && nrow(x$fishpop) == 0) {

    stop("No fish population was simulated.", call. = FALSE)

  }

  # plot summarized results
  if (summarize) {

    if (length(what) != 1) {

      stop("Please select only one 'what' argument.", call. = FALSE)

    }

    # set color for burn in threshold
    col_burn <- ifelse(test = x$burn_in, yes = "grey", no = NA)

    # summarize results
    data_sum <- summarize_mdlrn(result = x, what = what)[[1]]

    # get name of columns
    col_names <- names(data_sum)

    # create plot
    gg_top_left <- ggplot2::ggplot(data = data_sum) +
      ggplot2::geom_vline(xintercept = x$burn_in, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes_string(x = "timestep", y = col_names[2],
                                             col = "summary", linetype = "summary")) +
      ggplot2::scale_y_continuous(limits = limits$bg_biomass) +
      ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
      ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
      ggplot2::guides(col = "none", linetype = "none") +
      ggplot2::labs(x = "Timestep", y = col_names[2]) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    # create plot
    gg_top_right <- ggplot2::ggplot(data = data_sum) +
      ggplot2::geom_vline(xintercept = x$burn_in, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes_string(x = "timestep", y = col_names[3],
                                             col = "summary", linetype = "summary")) +
      ggplot2::scale_y_continuous(limits = limits$ag_biomass) +
      ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
      ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
      ggplot2::guides(col = "none", linetype = "none") +
      ggplot2::labs(x = "Timestep", y = col_names[3]) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    # create plot
    gg_bottom_left <- ggplot2::ggplot(data = data_sum) +
      ggplot2::geom_vline(xintercept = x$burn_in, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes_string(x = "timestep", y = col_names[4],
                                             col = "summary", linetype = "summary")) +
      ggplot2::scale_y_continuous(limits = limits$nutrients_pool) +
      ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
      ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
      ggplot2::guides(col = "none", linetype = "none") +
      ggplot2::labs(x = "Timestep", y = col_names[4]) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    # create plot
    gg_bottom_right <- ggplot2::ggplot(data = data_sum) +
      ggplot2::geom_vline(xintercept = x$burn_in, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes_string(x = "timestep", y = col_names[5],
                                             col = "summary", linetype = "summary")) +
      ggplot2::scale_y_continuous(limits = limits$detritus_pool) +
      ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
      ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
      ggplot2::guides(col = "none", linetype = "none") +
      ggplot2::labs(x = "Timestep", y = col_names[5]) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

  # plot map
  } else {

    if (what == "seafloor") {

      # get seafloor data
      seafloor <- subset(x$seafloor, timestep == max(timestep),
                         select = c("x", "y", "ag_biomass", "bg_biomass",
                                    "nutrients_pool", "detritus_pool"))

      # create plot
      gg_top_left <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bg_biomass)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", limits = limits$ag_biomass,
                                      name = "Dry weight bg\nbiomass [g/cell]") +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_top_right <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = ag_biomass)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", limits = limits$bg_biomass,
                                      name = "Dry weight ag\nbiomass [g/cell]") +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_bottom_left <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = nutrients_pool)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", limits = limits$nutrients_pool,
                                      name = "Nutrients\npool [g/cell]") +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_bottom_right <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = detritus_pool)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", limits = limits$detritus_pool,
                                      name = "Detritus\nnutrients [g/cell]") +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    # plot fishpopulation
    } else if (what == "fishpop") {

      # get seafloor data
      fishpop <- get_density(x, normalize = normalize, verbose = verbose)

      name <- ifelse(test = normalize, yes = "Density [#/cell/total time]",
                     no = "Density [#/cell]")

      # create title
      plot_title <- paste0("Total time : ", x$max_i, " iterations (",
                           round(x$max_i * x$min_per_i / 60 / 24, 1), " days)",
                           "\nFishpop    : ", x$starting_values$pop_n," indiv (movement: ", x$movement, ")")

      # create plot
      gg_density <- ggplot2::ggplot(data = fishpop) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = density)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", name = name) +
        ggplot2::coord_equal() +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::labs(title = plot_title) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      return(gg_density)

    # what doesn't make sense
    } else {

      stop("Please select 'seafloor' or 'fishpop' as 'what argument.",
           call. = FALSE)

    }
  }

  # create title
  plot_title <- paste0("Total time : ", x$max_i, " iterations (",
                       round(x$max_i * x$min_per_i / 60 / 24, 1), " days)",
                       "\nFishpop    : ", x$starting_values$pop_n," indiv (movement: '", x$movement, "')")


  # now add the title
  title <- cowplot::ggdraw() +
    cowplot::draw_label(label = plot_title,
                        x = 0, hjust = 0, size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 0,
                                                 b = 0, l = 1, "cm"))

  # combine to one grid
  gg_all <- cowplot::plot_grid(gg_top_left, gg_top_right,
                               gg_bottom_left, gg_bottom_right, nrow = 2, ncol = 2)

  # add title
  gg_all <- cowplot::plot_grid(title, gg_all, ncol = 1, rel_heights = c(0.1, 1))

  return(gg_all)

}
