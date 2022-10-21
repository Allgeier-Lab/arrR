### Arguments ####

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# setup dimensions
dimensions <- c(50, 50)

# set grain
grain <- 1

min_per_i <- 120

# setup iterations arguments
max_i <- (95 * 24) / (min_per_i / 60)

burn_in <- (5 * 24) / (min_per_i / 60)

save_each <- 24 / (min_per_i / 60)

# get stable values
stable_values <- arrR::get_req_nutrients(bg_biomass = arrR::default_starting$bg_biomass,
                                         ag_biomass = arrR::default_starting$ag_biomass,
                                         parameters = arrR::default_parameters)

#### Parameters ####

starting_values_stable <- arrR::default_starting
starting_values_stable$nutrients_pool <- stable_values$nutrients_pool
starting_values_stable$detritus_pool <- stable_values$detritus_pool

parameters_open <- arrR::default_parameters
parameters_open$nutrients_loss <- 0.1
nutrients_input <- rep(x = arrR::default_starting$nutrients_pool, times = max_i)

#### Seafloor ####

# create input seafloor
input_seafloor <- arrR::setup_seafloor(dimensions = dimensions, grain = grain,
                                       reef = reef_matrix, starting_values = arrR::default_starting)

# create input seafloor
input_seafloor_rnd <- arrR::setup_seafloor(dimensions = dimensions, grain = grain,
                                           reef = reef_matrix, starting_values = arrR::default_starting,
                                           random = 0.25)

input_seafloor_stable <- arrR::setup_seafloor(dimensions = dimensions, grain = grain,
                                              reef = NULL, starting_values = starting_values_stable,
                                              verbose = FALSE)

#### Fishpop ####

# create fishpop
input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor, starting_values = arrR::default_starting,
                                     parameters = arrR::default_parameters)

input_fishpop_unif <- arrR::setup_fishpop(seafloor = input_seafloor, starting_values = arrR::default_starting,
                                          parameters = arrR::default_parameters,
                                          use_log = FALSE)

#### Model runs ####

result_rand <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                    parameters = arrR::default_parameters, movement = "rand",
                                    max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                                    burn_in = burn_in)

result_attr <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                    parameters = arrR::default_parameters, movement = "attr",
                                    max_i = max_i, min_per_i = min_per_i, save_each = save_each)

result_behav <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                     parameters = arrR::default_parameters, movement = "behav",
                                     max_i = max_i, min_per_i = min_per_i, save_each = save_each)

result_stable <- arrR::run_simulation(seafloor = input_seafloor_stable, fishpop = NULL,
                                      parameters = arrR::default_parameters, movement = "rand",
                                      max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                                      burn_in = burn_in)

result_open <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                    parameters = parameters_open, movement = "rand",
                                    nutrients_input = nutrients_input,
                                    max_i = max_i, min_per_i = min_per_i, save_each = save_each)
