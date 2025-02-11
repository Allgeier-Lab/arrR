# set filter time step
filter_time <- c(0, max_i / 2)

# filter results
result_fltr <- arrR::filter_mdlrn(result = result_rand, filter = filter_time)

test_that("result_fltr returns mdl_rn", {

  expect_s3_class(object = result_fltr, class = "mdl_rn")

})

test_that("filter_mdlrn only return until timestep", {

  expect_lt(object = result_fltr$max_i, expected = result_rand$max_i)

  expect_equal(object = max(result_fltr$seafloor$timestep),
               expected = max(seq(from = 0, to = filter_time[2], by = save_each)))

  expect_equal(object = max(result_fltr$fishpop$timestep),
               expected = max(seq(from = 0, to = filter_time[2], by = save_each)))

})

test_that("filter_mdlrn returns error", {

  expect_error(object = arrR::filter_mdlrn(result = 1:5, filter = filter_time),
               regexp = "Please provide 'mdl_rn' object created with 'run_simulation'.")

  expect_error(object = arrR::filter_mdlrn(result = result_rand, filter = max_i + 1),
               regexp = "'filter' is not within 0 <= x <= max_i.")

  expect_error(object = arrR::filter_mdlrn(result = result_rand, filter = 51),
               regexp = "No iterations left after applying 'filter'.")

})
