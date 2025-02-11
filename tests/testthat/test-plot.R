test_that("plot.mdl_rn return ggplot", {

  expect_s3_class(plot(result_rand), class = "ggplot")

  expect_s3_class(plot(result_rand, summarize = TRUE), class = "ggplot")

  expect_s3_class(plot(result_rand, what = "fishpop", verbose = FALSE), class = "ggplot")

  expect_s3_class(plot(result_rand,what = "fishpop", summarize = TRUE), class = "ggplot")

})
