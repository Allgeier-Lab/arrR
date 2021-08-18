test_that("setup_fishpop creates data.frame", {

  expect_s3_class(object = input_fishpop, class = "data.frame")

  expect_s3_class(object = input_fishpop_unif, class = "data.frame")

})

test_that("setup_fishpop has correct dimensions", {

  expect_equal(object = nrow(input_fishpop),
               expected = starting_values$pop_n)

})

test_that("setup_fishpop uses log distribution of size", {

  size_unif <- mean(input_fishpop_unif$weight)

  size_log <- mean(input_fishpop$weight)

  expect_gt(object = size_unif, expected = size_log)

})

test_that("setup_fishpop can return no fish", {

  expect_equal(object = nrow(input_fishpop_null),
               expected = 0)

})
