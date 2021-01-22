# get parameters
parameters <- arrR::default_parameters

starting_values <- arrR::default_starting_values

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
                                    max_i = max_i, min_per_i = min_per_i,
                                    burn_in = 10)

test_that("run_simulation returns rnd_mdl", {

  expect_is(object = result_rand, class = "mdl_rn")

})

test_that("run_simulation contains seafloor and fishpop", {

  expect_equal(object = nrow(result_rand$seafloor),
               expected = max_i * raster::ncell(input_seafloor) + raster::ncell(input_seafloor))

  expect_equal(object = nrow(result_rand$fishpop),
               expected = max_i * nrow(input_fishpop) + nrow(input_fishpop))

  expect_equal(object = unique(result_rand$seafloor$timestep),
               expected = 0:max_i)

  expect_equal(object = unique(result_rand$fishpop$timestep),
               expected = 0:max_i)

})

test_that("run_simulation contains model run information", {

  expect_equal(object = result_rand$max_i, expected = max_i)

  expect_equal(object = result_rand$min_per_i, expected = min_per_i)

  expect_equal(object = result_rand$extent, expected = raster::extent(input_seafloor))

  expect_equal(object = result_rand$grain, expected = raster::res(input_seafloor))

  expect_equal(object = result_rand$reef_attraction, expected = FALSE)

  expect_equal(object = result_rand$burn_in, expected = 10)

})

test_that("run_simulation does not return burn_in", {

  result <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                 parameters = parameters,
                                 reef_attraction = FALSE,
                                 max_i = max_i, min_per_i = min_per_i,
                                 burn_in = 20, return_burnin = FALSE)

  expect_true(object = min(result$seafloor$timestep) == result$burn_in)

  expect_true(object = min(result$fishpop$timestep) == result$burn_in)

})

test_that("run_simulation stops if burn_in is out of limits", {

  expect_warning(arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                    parameters = parameters,
                                    reef_attraction = FALSE,
                                    max_i = max_i, min_per_i = min_per_i,
                                    burn_in = max_i + 1),
                 regexp = "'burn_in' larger than or equal to 'max_i' or 'burn_in' < 0.")

})

test_that("run_simulation stops max_i cannot be divided by save_each", {

  expect_error(arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                    parameters = parameters,
                                    reef_attraction = FALSE,
                                    max_i = max_i, save_each = 3),
               regexp = "'max_i' cannot be divided by 'save_each' without rest.")

})
