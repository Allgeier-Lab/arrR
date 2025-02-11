test_that("plot_allocation return ggplot", {

  expect_s3_class(arrR::plot_allocation(arrR::default_parameters), class = "ggplot")

})
