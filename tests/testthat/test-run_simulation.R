test_that("run_simulation returns rnd_mdl", {

  expect_s3_class(object = result_rand, class = "mdl_rn")

  expect_s3_class(object = result_attr, class = "mdl_rn")

  expect_s3_class(object = result_behav, class = "mdl_rn")

  expect_s3_class(object = result_rand_inout, class = "mdl_rn")

})

test_that("run_simulation contains seafloor and fishpop", {

  n_cells <- max_i * terra::ncell(input_seafloor) / save_each +
    terra::ncell(input_seafloor)

  n_fish <- max_i * nrow(input_fishpop) / save_each + nrow(input_fishpop)

  seq_i <- seq(from = 0, to = max_i, by = save_each)


  expect_equal(object = nrow(result_rand$seafloor), expected = n_cells)

  expect_equal(object = nrow(result_rand$fishpop), expected = n_fish)


  expect_equal(object = unique(result_rand$seafloor$timestep), expected = seq_i)

  expect_equal(object = unique(result_rand$fishpop$timestep), expected = seq_i)

})

test_that("run_simulation contains model run information", {

  expect_equal(object = names(result_rand),
               expected = c("seafloor", "fishpop", "movement", "max_dist", "pop_reserves_thres",
                            "nutr_input", "starting_values", "parameters", "coords_reef",
                            "extent", "grain", "dimensions", "max_i", "min_per_i",
                            "burn_in", "seagrass_each", "save_each"))


  expect_equal(object = result_rand$movement, expected = "rand")

  expect_equal(object = result_attr$movement, expected = "attr")

  expect_equal(object = result_behav$movement, expected = "behav")


  expect_equal(object = result_rand$nutr_input, expected = NA)

  expect_equal(object = result_rand_inout$nutr_input, expected = nutr_input)


  expect_equal(object = result_rand$extent, expected = terra::ext(input_seafloor))

  expect_equal(object = result_rand$dimensions, expected = dim(input_seafloor)[1:2])

  expect_equal(object = result_rand$grain, expected = terra::res(input_seafloor))


  expect_equal(object = result_rand$max_i, expected = max_i)

  expect_equal(object = result_rand$min_per_i, expected = min_per_i)

  expect_equal(object = result_rand$burn_in, expected = burn_in)

  expect_equal(object = result_rand$seagrass_each, expected = 1)

  expect_equal(object = result_rand$save_each, expected = save_each)

})

test_that("run_simulation uses correct movement behavior", {

  density_rand <- arrR::get_density(result = result_rand, verbose = FALSE)

  density_attr <- arrR::get_density(result = result_attr, verbose = FALSE)

  density_behav <- arrR::get_density(result = result_behav, verbose = FALSE)


  n_rand <- sum(head(sort(density_rand$density, decreasing = TRUE)))

  n_attr <- sum(head(sort(density_attr$density, decreasing = TRUE)))

  n_behav <- sum(head(sort(density_behav$density, decreasing = TRUE)))


  expect_gt(object = n_attr, expected = n_rand)

  expect_gt(object = n_behav, expected = n_rand)

})

test_that("run_simulation stops max_i cannot be divided by save_each", {

  expect_error(arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                    parameters = parameters,
                                    movement = "rand",
                                    max_i = max_i, save_each = 3),
               regexp = "'max_i' cannot be divided by 'save_each' without rest.")

})

test_that("run_simulation stops if nutr_input is not equal to max_i", {

  expect_error(arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
                                    parameters = parameters,
                                    nutr_input = c(0.1, 0.2, 0.3),
                                    movement = "rand",
                                    max_i = max_i, min_per_i = min_per_i),
               regexp = "'nutr_input' must have input amount for each iteration.")

})

# test_that("run_simulation does not return burn_in", {
#
#   result <- arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
#                                  parameters = parameters,
#                                  movement = "rand",
#                                  max_i = max_i, min_per_i = min_per_i,
#                                  burn_in = 10, return_burnin = FALSE, verbose = FALSE)
#
#   expect_true(object = min(result$seafloor$timestep) == result$burn_in)
#
#   expect_true(object = min(result$fishpop$timestep) == result$burn_in)
#
# })

# test_that("run_simulation stops if burn_in is out of limits", {
#
#   expect_warning(arrR::run_simulation(seafloor = input_seafloor, fishpop = input_fishpop,
#                                       parameters = parameters,
#                                       movement = "rand",
#                                       max_i = max_i, min_per_i = min_per_i,
#                                       burn_in = max_i + 1),
#                  regexp = "'burn_in' larger than or equal to 'max_i' or 'burn_in' < 0. Setting to burn_in = 0")
#
# })
