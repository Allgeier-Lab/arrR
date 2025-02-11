# get densities
density_rand <- arrR::get_density(result = result_rand, verbose = FALSE)

density_attr <- arrR::get_density(result = result_attr, verbose = FALSE)

density_attr_norm <- arrR::get_density(result = result_attr, normalize = TRUE,
                                       verbose = FALSE)

test_that("get_density returns data.frame", {

  expect_s3_class(object = density_rand, class = "data.frame")

})

test_that("get_density returns value for each cell", {

  expect_equal(density_rand$x, expected = input_seafloor$x)

  expect_equal(density_rand$y, expected = input_seafloor$y)

})

test_that("get_density returns normalized results", {

  expect_equal(object = density_attr$density / max_i, expected = density_attr_norm$density)

})

test_that("get_density returns error", {

 expect_error(object = arrR::get_density(result = c(1, 2, 3)),
              regexp = "Please provide 'mdl_rn' object createt with run_simulation.")

})
