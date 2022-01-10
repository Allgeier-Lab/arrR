# get stable values
stable_vals <- arrR::get_stable_values(bg_biomass = starting_values$bg_biomass,
                                       ag_biomass = starting_values$ag_biomass,
                                       parameters = parameters, verbose = FALSE)

test_that("get_stable_values returns lists", {

  expect_type(object = stable_vals, type = "list")

})

test_that("get_stable_values returns all values", {

  expect_equal(object = names(stable_vals), expected = c("nutrients_pool", "detritus_pool",
                                                         "nutr_input"))

})
