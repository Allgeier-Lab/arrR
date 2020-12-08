parameters <- arrR::read_parameters("parameters.csv", sep = ";")

starting_values <- arrR::read_parameters("starting_values.csv", sep = ";")

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# create input seafloor
input_seafloor <- arrR::setup_seafloor(extent = c(50, 50), grain =  c(1, 1),
                                       reefs = reef_matrix,
                                       starting_values = starting_values,
                                       random = 0.0)

input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                     starting_values = starting_values,
                                     parameters = parameters,
                                     use_log = FALSE)

input_fishpop_log <- arrR::setup_fishpop(seafloor = input_seafloor,
                                         starting_values = starting_values,
                                         parameters = parameters,
                                         use_log = TRUE)

max_i <- 100

min_per_i <- 120

result_rand <- arrR::run_simulation(seafloor = input_seafloor,
                                    fishpop  = input_fishpop,
                                    parameters = parameters,
                                    reef_attraction = FALSE,
                                    max_i = max_i, min_per_i = min_per_i)

filter_time <- 50

result_fltr <- filter_mdlrn(result = result_rand, timestep = filter_time)

test_that("filter_mdlrn only return until timestep", {

  expect_lt(object = result_fltr$max_i, expected = result_rand$max_i)

  expect_equal(object = max(result_fltr$seafloor$timestep), expected = filter_time)

  expect_equal(object = max(result_fltr$fishpop$timestep), expected = filter_time)

})
