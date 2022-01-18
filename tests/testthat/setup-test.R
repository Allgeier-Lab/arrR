library(dplyr)
library(terra)

# get parameters
parameters <- arrR::arrR_parameters

parameters_output <- parameters

parameters_output$nutrients_loss <- 0.01

# get starting values
starting_values <- arrR::arrR_starting_values

starting_values_null <- arrR::arrR_starting_values

starting_values_null$pop_n <- 0

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# setup dimensions
dimensions <- c(50, 50)

# set grain
grain <- c(1, 1)

# setup iterations arguments
max_i <- 1000

min_per_i <- 120

burn_in <- 5

save_each <- 10

# create nutrient input
nutrients_input <- rep(x = starting_values$nutrients_pool, times = max_i)

# create input seafloor
input_seafloor <- arrR::setup_seafloor(dimensions = dimensions, grain = grain,
                                       reef = reef_matrix, starting_values = starting_values,
                                       verbose = FALSE)

input_seafloor_rnd <- arrR::setup_seafloor(dimensions = dimensions, grain = grain,
                                           reef = reef_matrix,
                                           starting_values = starting_values,
                                           random = 0.25, verbose = FALSE)

# create fishpop
input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                     starting_values = starting_values,
                                     parameters = parameters, verbose = FALSE)

input_fishpop_unif <- arrR::setup_fishpop(seafloor = input_seafloor,
                                          starting_values = starting_values,
                                          parameters = parameters,
                                          use_log = FALSE, verbose = FALSE)

input_fishpop_null <- arrR::setup_fishpop(seafloor = input_seafloor,
                                          starting_values = starting_values_null,
                                          parameters = parameters,
                                          use_log = TRUE, verbose = FALSE)

# run model
result_rand <- arrR::run_simulation(seafloor = input_seafloor, fishpop  = input_fishpop,
                                    parameters = parameters, movement = "rand",
                                    max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                                    burn_in = burn_in, verbose = FALSE)

# run model
result_rand_inout <- arrR::run_simulation(seafloor = input_seafloor, fishpop  = input_fishpop,
                                          parameters = parameters_output, movement = "rand",
                                          nutrients_input = nutrients_input,
                                          max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                                          burn_in = burn_in, verbose = FALSE)

# run model with attr movement
result_attr <- arrR::run_simulation(seafloor = input_seafloor, fishpop  = input_fishpop,
                                    parameters = parameters, movement = "attr",
                                    max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                                    burn_in = burn_in, verbose = FALSE)

# run model with attr movement
result_behav <- arrR::run_simulation(seafloor = input_seafloor, fishpop  = input_fishpop,
                                     parameters = parameters, movement = "behav",
                                     max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                                     burn_in = burn_in, verbose = FALSE)
