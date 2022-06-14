## code to prepare `DATASET` dataset goes here
library(arrR)

# get starting values
starting_values <- arrR::default_starting

# get parameters
parameters <- arrR::default_parameters

# change some starting values and parameters
starting_values$pop_n <- 8

parameters$pop_reserves_max <- c(0.1, 0.15)

parameters$seagrass_thres <- -1/4

# create 5 reef cells in center of seafloor
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# get stable value
stable_values <- get_req_nutrients(bg_biomass = starting_values$bg_biomass,
                                   ag_biomass = starting_values$ag_biomass,
                                   parameters = parameters)

starting_values$nutrients_pool <- stable_values$nutrients_pool

starting_values$detritus_pool <- stable_values$detritus_pool

# create seafloor
input_seafloor <- setup_seafloor(dimensions = c(50, 50), grain = 1,
                                 reef = reef_matrix, starting_values = starting_values,
                                 random = 0.05)

# create fishpop
input_fishpop <- setup_fishpop(seafloor = input_seafloor, species = c(2, 2, 1, 2, 1, 1, 2, 2),
                               starting_values = starting_values, parameters = parameters)

# setup iterations things
min_per_i <- 120

# run the model for 10 years
years <- 10
max_i <- (60 * 24 * 365 * years) / min_per_i

# run seagrass once each day
days <- 1
seagrass_each <- (24 / (min_per_i / 60)) * days

# save results only every 365 days
days <- 365
save_each <- (24 / (min_per_i / 60)) * days

max_i %% save_each

# run model
result_readme <- run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                parameters = parameters, movement = "attr",
                                max_i = max_i, min_per_i = min_per_i,
                                seagrass_each = seagrass_each, save_each = save_each)

usethis::use_data(result_readme, internal = TRUE, overwrite = TRUE)
