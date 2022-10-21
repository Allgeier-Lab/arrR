test_that("get_req_nutrients returns lists", {

  expect_type(object = stable_values, type = "list")

})

test_that("get_req_nutrients returns all values", {

  expect_equal(object = names(stable_values), expected = c("nutrients_pool", "detritus_pool"))

})

test_that("get_req_nutrients keeps system stable", {

  expect_length(object = unique(signif(result_stable$seafloor$ag_biomass)), n = 1)

  expect_length(object = unique(signif(result_stable$seafloor$bg_biomass)), n = 1)

  expect_length(object = unique(signif(result_stable$seafloor$nutrients_pool)), n = 1)

  expect_length(object = unique(signif(result_stable$seafloor$detritus_pool)), n = 1)

})
