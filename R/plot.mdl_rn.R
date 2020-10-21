#' plot.mdl_rn
#'
#' @description Plotting method for mdl_rn object
#'
#' @param x mdl_rn object of simulation run.
#' @param what Character specifying what to plot.
#' @param summarize Character to specify which values of environmental data is used as fill.
#' @param timestep Integer to specify which timestep is plotted.
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
plot.mdl_rn <- function(x, what, summarize = FALSE, timestep = x$max_i,
                        base_size = 10, ...) {

  i <- timestep

  # check if i can be divided by save_each without reminder
  if (i %% x$save_each != 0) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)
  }

  # plot summarized results
  if (summarize) {

    # summarize results
    summarised_result <- summarize_results(result = x)

    # plot seafloor
    if (what == "seafloor") {

      # get seafloor data
      seafloor <- subset(summarised_result$seafloor,
                         select = c("timestep", "summary",
                                    "ag_biomass", "bg_biomass",
                                    "nutrients_pool", "detritus_pool"))

      # reshape to long
      seafloor <- stats::reshape(data = seafloor, idvar = c("timestep", "summary"),
                                 varying = c("ag_biomass", "bg_biomass",
                                             "nutrients_pool", "detritus_pool"),
                                 times = c("ag_biomass", "bg_biomass",
                                           "nutrients_pool", "detritus_pool"),
                                 v.names = "value", timevar = "type", direction = "long")

      # make sure ordering is identical
      seafloor$type <- factor(seafloor$type,
                              levels = c("ag_biomass", "bg_biomass",
                                         "nutrients_pool", "detritus_pool"))

      # make sure ordering is identical
      seafloor$summary <- factor(seafloor$summary,
                              levels = c("min", "mean", "max"))

      # create plot
      gg_seafloor <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = value,
                                        col = summary, linetype = summary)) +
        ggplot2::facet_wrap(~ type, scales = "free_y") +
        ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
        ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
        ggplot2::guides(col = FALSE, linetype = FALSE) +
        ggplot2::labs(title = paste0("Simulation time: ",
                                     round(i * x$min_per_i / 60 / 24, 1),
                                     " days\n(Timesteps: ", i, ")")) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      return(gg_seafloor)

    # plot fish population
    } else if (what == "fish_population") {

      # get fish  data
      fish_population <- subset(summarised_result$fish_population,
                         select = c("timestep" ,"summary",
                                    "length", "weight",
                                    "died_consumption", "died_background"))

      # reshape to long
      fish_population <- stats::reshape(data = fish_population, idvar = c("timestep", "summary"),
                                        varying = c("length", "weight",
                                                    "died_consumption", "died_background"),
                                        times = c("length", "weight",
                                                  "died_consumption", "died_background"),
                                        v.names = "value", timevar = "type", direction = "long")

      # make sure ordering is identical
      fish_population$type <- factor(fish_population$type,
                                     levels = c("length", "weight",
                                                "died_consumption", "died_background"))

      # make sure ordering is identical
      fish_population$summary <- factor(fish_population$summary,
                                     levels = c("min", "mean", "max"))

      # create plot
      gg_fish <- ggplot2::ggplot(data = fish_population) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = value,
                                        col = summary, linetype = summary)) +
        ggplot2::facet_wrap(~ type, scales = "free_y") +
        ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
        ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
        ggplot2::guides(col = FALSE, linetype = FALSE) +
        ggplot2::labs(title = paste0("Simulation time: ",
                                     round(i * x$min_per_i / 60 / 24, 1),
                                     " days\n(Timesteps: ", i, ")")) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      return(gg_fish)

    # waht doesn't make sense
    } else {

      stop("Please select 'seafloor' or 'fish_population' as 'what argument.",
           call. = FALSE)

    }

  # plot map
  } else {

    if (what == "seafloor") {

      # get seafloor data
      seafloor <- subset(x$seafloor, timestep == i,
                         select = c("x", "y",
                                    "ag_biomass", "bg_biomass",
                                    "nutrients_pool", "detritus_pool"))

      # create plot
      gg_ag_biomass <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = ag_biomass)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A") +
        ggplot2::coord_equal() +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_bg_biomass <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bg_biomass)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A") +
        ggplot2::coord_equal() +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_nutrients <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = nutrients_pool)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A") +
        ggplot2::coord_equal() +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_detritus <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = detritus_pool)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A") +
        ggplot2::coord_equal() +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # now add the title
      title <- cowplot::ggdraw() +
        cowplot::draw_label(label = paste0("Simulation time: ",
                                           round(i * x$min_per_i / 60 / 24, 1),
                                           " days\n(Timesteps: ", i, ")"),
                            x = 0, hjust = 0, size = base_size) +
        ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 0,
                                                     b = 0, l = 2.5, "cm"))

      # combine to one grid
      gg_all <- cowplot::plot_grid(gg_ag_biomass, gg_bg_biomass,
                                   gg_nutrients, gg_detritus, nrow = 2, ncol = 2)

      # add title
      gg_all <- cowplot::plot_grid(title, gg_all, ncol = 1, rel_heights = c(0.1, 1))

      return(gg_all)

    # plot fishpopulation
    } else if (what == "fish_population") {

      stop("Currently there is no visualization for the fish population if not summarized.",
           call. = FALSE)

    # what doesn't make sense
    } else {

      stop("Please select 'seafloor' or 'fish_population' as 'what argument.",
           call. = FALSE)

    }
  }
}
