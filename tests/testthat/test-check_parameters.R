test_that("check_parameters checks starting values and parameters only", {

  expect_message(object = arrR::check_parameters(starting_values = arrR::default_starting))

  expect_message(object = arrR::check_parameters(parameters = arrR::default_parameters))

})

test_that("check_parameters returns warning for ratios", {

  parameters <- arrR::default_parameters

  parameters$seagrass_slough <- 1.1

  expect_warning(object = arrR::check_parameters(parameters = parameters),
                 regexp = "Some parameters that must be 0 <= x <= 1 are outside range.")

})

test_that("check_parameters returns warning for biomass", {

  parameters <- arrR::default_parameters

  parameters$bg_biomass_min <- parameters$bg_biomass_max * 2

  expect_warning(object = arrR::check_parameters(parameters = parameters),
                 regexp = "Some minimum parameters are larger than maximum parameters.")

})

test_that("check_parameters returns warning for biomass", {

  parameters <- arrR::default_parameters

  parameters$pop_a <- -0.1

  expect_warning(object = arrR::check_parameters(parameters = parameters),
                 regexp = "'pop_a' must be positive number.")

})

test_that("check_parameters returns warning for missing parameters", {

  parameters <- arrR::default_parameters
  starting_values <- arrR::default_starting

  parameters <- parameters[-1]
  starting_values <- starting_values[-1]

  expect_warning(object = arrR::check_parameters(parameters = parameters),
                 regexp = "Missing parameter values: bg_biomass_min")

  expect_warning(object = arrR::check_parameters(starting_values = starting_values),
                 regexp = "Missing starting values: bg_biomass")

})

test_that("check_parameters returns message for additional values", {

  parameters <- arrR::default_parameters
  starting_values <- arrR::default_starting

  parameters$swim <- 2.5
  starting_values$swim <- 2.5

  expect_message(object = arrR::check_parameters(parameters = parameters),
                 regexp = "Not needed parameter values: swim")

  expect_message(object = arrR::check_parameters(starting_values = starting_values),
                 regexp = "Not needed starting values: swim")

})
