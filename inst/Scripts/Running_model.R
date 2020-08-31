# code to play around with model
library(coRal)
library(raster)
# library(tidyverse)

# load parameter and starting value files
starting_values <- system.file("extdata", "starting_values.csv", package = "coRal")
parameters <- system.file("extdata", "parameters.csv", package = "coRal")

starting_values <- read_parameters(file = starting_values, sep = ";")
parameters <- read_parameters(file = parameters, sep = ";")

# check if all parameters are present
check_parameters(starting_values = starting_values, parameters = parameters)

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# create seafloor
input_seafloor <- setup_seafloor(extent = c(50, 50), grain = 1, reefs = reef_matrix,
                                 starting_values = starting_values, parameters = parameters)

# # # change some parameters
# starting_values$pop_n <- 20

# create population
input_fish_population <- setup_fish_population(seafloor = input_seafloor,
                                               starting_values = starting_values,
                                               parameters = parameters)

# run model
result <- run_simulation(seafloor = input_seafloor, fish_population = input_fish_population,
                         starting_values = starting_values, parameters = parameters,
                         reef_attraction = TRUE,
                         max_i = 50, verbose = TRUE)

animate_result(result = result, fill = "ag_biomass")

# result$seafloor %>% dplyr::filter(track_i == 20) %>%
#   dplyr::pull(ag_biomass) %>%
#   mean
