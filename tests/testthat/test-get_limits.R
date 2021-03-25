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

result_a <- arrR::run_simulation(seafloor = input_seafloor,
                                 fishpop  = input_fishpop,
                                 parameters = parameters,
                                 reef_attraction = FALSE,
                                 max_i = max_i, min_per_i = min_per_i)

result_b <- arrR::run_simulation(seafloor = input_seafloor,
                                 fishpop  = input_fishpop,
                                 parameters = parameters,
                                 reef_attraction = FALSE,
                                 max_i = max_i, min_per_i = min_per_i)

limits_list <- get_limits(result = list(result_a, result_b))

test_that("get_limits returns list", {

  expect_type(object = limits_list, type = "list")

})

test_that("get_limits returns min and max", {

  expect_lte(object = limits_list$ag_biomass[1],
             expected = limits_list$ag_biomass[2])

  expect_lte(object = limits_list$bg_biomass[1],
             expected = limits_list$bg_biomass[2])

  expect_lte(object = limits_list$nutrients_pool[1],
             expected = limits_list$nutrients_pool[2])

  expect_lte(object = limits_list$detritus_pool[1],
             expected = limits_list$detritus_pool[2])

})

test_that("get_limits returns error", {

  expect_error(object = get_limits(result = list(c(1:5), c(5:10))),
               regexp = "Please prove mdl_rn object createt with run_simulation.")

})
