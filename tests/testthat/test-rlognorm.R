mean <- 5
sd <- 2
n <- 1000

result <- rlognorm(n = n, mean = mean, sd = sd)

test_that("rlognorm returns correct result", {

  expect_length(object = result, n = n)

  check_mean <- mean(result) < mean * 1.1 && mean(result) > mean * 0.9

  check_sd <- sd(result) < sd * 1.1 && sd(result) > sd * 0.9


  expect_true(object = check_mean)

  expect_true(object = check_sd)

})
