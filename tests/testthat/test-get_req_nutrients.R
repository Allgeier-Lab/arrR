# get stable values
stable_vals <- arrR::get_req_nutrients(bg_biomass = starting_values$bg_biomass,
                                       ag_biomass = starting_values$ag_biomass,
                                       parameters = parameters)

test_that("get_req_nutrients returns lists", {

  expect_type(object = stable_vals, type = "list")

})

test_that("get_req_nutrients returns all values", {

  expect_equal(object = names(stable_vals), expected = c("nutrients_pool", "detritus_pool"))

})

test_that("get_req_nutrients keeps system stable", {

  starting_values$nutrients_pool <- stable_vals$nutrients_pool

  starting_values$detritus_pool <- stable_vals$detritus_pool

  input_seafloor_null <- arrR::setup_seafloor(dimensions = dimensions, grain = grain,
                                              reef = NULL, starting_values = starting_values,
                                              verbose = FALSE)

  result_stable <- arrR::run_simulation(seafloor = input_seafloor_null, fishpop  = NULL,
                                        parameters = parameters, movement = "rand",
                                        max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                                        burn_in = burn_in, verbose = FALSE)

  expect_length(object = unique(signif(result_stable$seafloor$ag_biomass)), n = 1)

  expect_length(object = unique(signif(result_stable$seafloor$bg_biomass)), n = 1)

  expect_length(object = unique(signif(result_stable$seafloor$nutrients_pool)), n = 1)

  expect_length(object = unique(signif(result_stable$seafloor$detritus_pool)), n = 1)

})
