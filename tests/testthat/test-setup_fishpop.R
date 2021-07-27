# get parameters
parameters <- arrR::arrR_parameters

starting_values <- arrR::arrR_starting_values

starting_values_null <- starting_values

starting_values_null$pop_n <- 0

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

input_fishpop_null <- arrR::setup_fishpop(seafloor = input_seafloor,
                                          starting_values = starting_values_null,
                                          parameters = parameters,
                                          use_log = TRUE)

test_that("setup_fishpop creates data.frame", {

  expect_is(object = input_fishpop, class = "data.frame")

})

test_that("setup_fishpop has correct dimensions", {

  expect_equal(object = nrow(input_fishpop),
               expected = starting_values$pop_n)

})

test_that("setup_fishpop uses log distribution of size", {

  size_unif <- mean(input_fishpop$weight)

  size_log <- mean(input_fishpop_log$weight)

  expect_gt(object = size_unif, expected = size_log)

})

test_that("setup_fishpop can return no fish", {

  expect_equal(object = nrow(input_fishpop_null),
               expected = 0)

})
