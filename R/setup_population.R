#' setup_population
#'
#' @description Initiate fish population.
#'
#' @param seafloor Environment created with \code{\link{setup_environment}}.
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Function to setup the environment.....
#' Center of the environment is always set to (0,0).
#'
#' Parameters include ...
#'
#' @return data.table
#'
#' @examples
#' reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' starting_values <- system.file("extdata", "starting_values.csv", package = "coRal")
#' parameters <- system.file("extdata", "parameters.csv", package = "coRal")
#' starting_values <- read_parameters(file = starting_values, sep = ";")
#' parameters <- read_parameters(file = parameters, sep = ";")
#'
#' input_environment <- setup_environment(extent = c(100, 100), grain = 1,
#' reefs = reef_matrix, starting_values = starting_values, parameters = parameters)
#'
#' input_population <- setup_population(seafloor = input_environment,
#' starting_values = starting_values, parameters = parameters)
#'
#' @aliases setup_population
#' @rdname setup_population
#'
#' @export
setup_population <- function(seafloor, starting_values, parameters, verbose = TRUE) {

  n <- starting_values$pop_n

  if (verbose) {

    message("Creating ", n, " individuals within ", raster::extent(seafloor), "...")

  }

  # create random coordinates within environment
  x <- stats::runif(n = n, min = raster::xmin(seafloor),
                    max = raster::xmax(seafloor))

  y <- stats::runif(n = n, min = raster::ymin(seafloor),
                    max = raster::ymax(seafloor))

  # calculate length and weight
  size <- int_calc_size(starting_values = starting_values,
                        parameters = parameters)

  # MH: What is this?
  # MH: Why are this values not treated as parameters?
  # MH: aen=N assimilation efficiency?
  # MH: But if its identical all the time, no need to store it for each individual
  n_body <- 2.999
  aen <-  0.75

  # MH: Where do these formula come from?
  # MH: Why is reserves_max = reserves?
  reserves_max <- n_body / 100 * size$weight * 0.05
  reserves <- n_body / 100 * size$weight * 0.05

  # combine to final data frame
  population <- data.table::data.table(id = 1:n, age = 0,
                                       x = x, y = y,
                                       length = size$length, weight = size$weight,
                                       aen = aen, n_body = n_body,
                                       reserves = reserves, reserves_max = reserves_max,
                                       reserves_diff = reserves_max - reserves,
                                       activity = numeric(n), respiration = numeric(n),
                                       growth_length = numeric(n), growth_weight = numeric(n),
                                       growth_nutrient = numeric(n), consumption_req = numeric(n),
                                       died = numeric(n))

  return(population)
}

# turtles-own [ ;;these are fish specific variables
#               aen                  ; species specific N assimilation efficiency
#               Nbody                ; species specific N of body
#               weight               ; weight of fish (g)
#               body-length-current  ;current length of fish (cm)
#               body-length-old      ;length of fish in previous timestep [t-1] (cm)
#               age                  ; age of fish (min)
#               Cmax                 ; Max consumption depending on length
#               Activity             ; Activity level depending on movement
#               travel-distance      ; distance travelled at time-step to determine activity
#               resp                 ; respiration at time step
#               prop.cons            ; proportion of C.max actually consumed
#               consumption-required ; actual consmption in g(fish) required for growth
#               nutrient-excretion   ; output of bioenergetics of nutrients excreted to environment
#               death-prob           ; probability of 'death' or reset of body size
#
#               reserves-max         ; this is our theoretical 'doggy bag' - I've dubbed it 'reserves' as a place where the fish could get the nutrients necessary to not have negative growth.
#   reserves             ; if these reserves go to 0, then the fish 'dies', resetting the biomass
#   reserves-new
#
#   Cn.desired
#   Cn.real
#
#   reset-count
#   reserve-reset-count
#
#   death-nutrient-contribution
#
#   growth-amount
#   growth-amount-weight
#   growth-nutrient
#   egestion-nutrient
#   new-size
#   body-length-start      ;;this is the initial size of the turtle
#
#   reserves.old
#
# ]
