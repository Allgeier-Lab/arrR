test_that("setup_seafloor has correct dimensions", {

  seafloor_dim <- arrR:::get_seafloor_dim(input_seafloor)

  expect_equal(object = nrow(input_seafloor),
               expected = dimensions[[1]] * dimensions[[2]])

  expect_equal(object = unname(seafloor_dim$grain),
               expected = grain)

})

test_that("setup_seafloor includes reef cells", {

  expect_equal(object = sum(input_seafloor$reef),
               expected = nrow(reef_matrix))

})

test_that("setup_seafloor has correct starting values", {

  expect_equal(object = mean(input_seafloor$ag_biomass, na.rm = TRUE),
               expected = arrR::default_starting$ag_biomass)

  expect_equal(object =  mean(input_seafloor$bg_biomass, na.rm = TRUE),
               expected = arrR::default_starting$bg_biomass)

  expect_equal(object = mean(input_seafloor$nutrients_pool, na.rm = TRUE),
               expected = arrR::default_starting$nutrients_pool)

  expect_equal(object = mean(input_seafloor$detritus_pool, na.rm = TRUE),
               expected = arrR::default_starting$detritus_pool)

})

test_that("setup_seafloor adds random noise", {

  range_homo <- range(input_seafloor$ag_biomass, na.rm = TRUE)

  range_random <- range(input_seafloor_rnd$ag_biomass, na.rm = TRUE)


  expect_equal(object = range_homo[[1]],
               expected = range_homo[[2]])

  expect_lt(object = range_random[[1]],
            expected = range_random[[2]])

})

test_that("setup_seafloor returns error", {

  expect_error(object = arrR::setup_seafloor(dimensions = dimensions, grain = grain,
                                             reef = 5, starting_values = arrR::default_starting,
                                             random = 0.25),
               regexp = "Please provide a 2-column with x,y coordinates of reef cells.")

  expect_error(object = arrR::setup_seafloor(dimensions = dimensions, grain = grain,
                                             reef = cbind(reef_matrix, 5),
                                             starting_values = arrR::default_starting,
                                             random = 0.25),
               regexp = "Please provide a 2-column with x,y coordinates of reef cells.")

})
