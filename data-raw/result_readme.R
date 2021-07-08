## code to prepare `DATASET` dataset goes here
library(arrR)

starting_values <- arrR::default_starting_values

parameters <- arrR::default_parameters

check_parameters(starting_values = starting_values, parameters = parameters)

parameters$pop_reserves_max <- 0.1

reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

input_seafloor <- setup_seafloor(extent = c(50, 50), grain = 1,
                                 reefs = reef_matrix,
                                 starting_values = starting_values)

input_fishpop <- setup_fishpop(seafloor = input_seafloor,
                               starting_values = starting_values,
                               parameters = parameters)

min_per_i <- 120

# run the model for three years
years <- 10
max_i <- (60 * 24 * 365 * years) / min_per_i

# save results only every 15 days
days <- 25
save_each <- (24 / (min_per_i / 60)) * days

# run model
result <- run_simulation(seafloor = input_seafloor,
                         fishpop = input_fishpop,
                         parameters = parameters,
                         movement = "attr",
                         max_i = max_i, min_per_i = min_per_i,
                         save_each = save_each)

usethis::use_data(result, internal = TRUE)