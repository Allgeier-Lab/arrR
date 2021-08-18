# get stable values
stable_vals <- arrR::get_stable_values(starting_values = starting_values,
                                       parameters = parameters, verbose = FALSE)

stable_vals_fishpop <- arrR::get_stable_values(starting_values = starting_values,
                                               parameters = parameters,
                                               fishpop = TRUE, min_per_i = min_per_i,
                                               verbose = FALSE)

test_that("get_stable_values returns lists", {

  expect_type(object = stable_vals, type = "list")

})

test_that("get_stable_values returns all values", {

  expect_equal(object = names(stable_vals), expected = c("nutrients_pool", "detritus_pool",
                                                         "nutr_input"))

})

test_that("get_stable_values returns higher values if fish are considered", {

  expect_gt(object = stable_vals_fishpop$detritus_pool, expected = stable_vals$detritus_pool)

})
