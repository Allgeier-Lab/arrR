# get parameters
parameters <- arrR::arrR_parameters

starting_values <- arrR::arrR_starting_values

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# create input seafloor
input_seafloor <- arrR::setup_seafloor(dimensions = c(50, 50), grain = c(1, 1),
                                       reefs = reef_matrix, starting_values = starting_values)

input_fishpop <- arrR::setup_fishpop(seafloor = input_seafloor,
                                     starting_values = starting_values,
                                     parameters = parameters)

max_i <- 100

min_per_i <- 120

result_rand <- arrR::run_simulation(seafloor = input_seafloor, fishpop  = input_fishpop,
                                    parameters = parameters, movement = "rand",
                                    max_i = max_i, min_per_i = min_per_i)

test_that("plot.mdl_rn return ggplot", {

  expect_s3_class(plot(result_rand), class = "ggplot")

  expect_s3_class(plot(result_rand, summarize = TRUE), class = "ggplot")

  expect_s3_class(plot(result_rand, what = "fishpop"), class = "ggplot")

  expect_s3_class(plot(result_rand,what = "fishpop", summarize = TRUE), class = "ggplot")

})
