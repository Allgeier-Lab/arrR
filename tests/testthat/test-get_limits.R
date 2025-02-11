# get limits
limits_list <- arrR::get_limits(result = list(result_rand, result_attr))

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

  expect_error(object = arrR::get_limits(result = list(c(1:5), c(5:10))),
               regexp = "Please provide 'mdl_rn' object createt with 'run_simulation()'.",
               fixed = TRUE)

})
