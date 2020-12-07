starting_values <- arrR::read_parameters("starting_values.csv", sep = ";")

starting_values_df <- arrR::read_parameters("starting_values.csv", sep = ";",
                                            return_list = FALSE)

test_that("read_parameters returns list and data.frame", {

  expect_type(object = starting_values, type = "list")

  expect_s3_class(object = starting_values_df, class = "data.frame")

})
