test_that("plot_allocation return ggplot", {

  expect_s3_class(arrR::plot_allocation(parameters), class = "ggplot")

})
