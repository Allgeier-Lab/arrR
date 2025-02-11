test_that("setup_fishpop creates data.frame", {

  expect_s3_class(object = input_fishpop, class = "data.frame")

  expect_s3_class(object = input_fishpop_unif, class = "data.frame")

})

test_that("setup_fishpop has correct dimensions", {

  expect_equal(object = nrow(input_fishpop),
               expected = arrR::default_starting$pop_n)

})

test_that("setup_fishpop can return no fish", {

  starting_values <- arrR::default_starting

  starting_values$pop_n <- 0

  input_fishpop_null <- arrR::setup_fishpop(seafloor = input_seafloor, starting_values = starting_values,
                                            parameters = arrR::default_parameters,
                                            use_log = TRUE, verbose = FALSE)

  expect_equal(object = nrow(input_fishpop_null),
               expected = 0)

})
