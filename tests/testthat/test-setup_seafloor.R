test_that("setup_seafloor creates SpatRaster", {

  expect_s4_class(object = input_seafloor, class = "SpatRaster")

})

test_that("setup_seafloor has correct dimensions", {

  expect_equal(object = terra::ncell(input_seafloor),
               expected = dimensions[[1]] * dimensions[[2]])

  expect_equal(object = terra::res(input_seafloor),
               expected = grain)

})

test_that("setup_seafloor includes reef cells", {

  expect_equal(object = sum(terra::values(input_seafloor$reef, mat = FALSE)),
               expected = nrow(reef_matrix))

})

test_that("setup_seafloor has correct starting values", {

  expect_equal(object = mean(terra::values(input_seafloor$ag_biomass, mat = FALSE), na.rm = TRUE),
               expected = starting_values$ag_biomass)

  expect_equal(object = mean(terra::values(input_seafloor$bg_biomass, mat = FALSE), na.rm = TRUE),
               expected = starting_values$bg_biomass)

  expect_equal(object = mean(terra::values(input_seafloor$nutrients_pool, mat = FALSE), na.rm = TRUE),
               expected = starting_values$nutrients_pool)

  expect_equal(object = mean(terra::values(input_seafloor$detritus_pool, mat = FALSE), na.rm = TRUE),
               expected = starting_values$detritus_pool)

})

test_that("setup_seafloor adds random noise", {

  range_homo <- range(terra::values(input_seafloor$ag_biomass, mat = FALSE), na.rm = TRUE)

  range_random <- range(terra::values(input_seafloor_rnd$ag_biomass, mat = FALSE), na.rm = TRUE)


  expect_equal(object = range_homo[[1]],
               expected = range_homo[[2]])

  expect_lt(object = range_random[[1]],
            expected = range_random[[2]])

})

test_that("setup_seafloor returns error", {

  expect_error(object = arrR::setup_seafloor(dimensions = dimensions, grain = grain,
                                             reef = 5, starting_values = starting_values,
                                             random = 0.25),
               regexp = "Please provide a 2-column with x,y coordinates of reef cells.")

  expect_error(object = arrR::setup_seafloor(dimensions = dimensions, grain = grain,
                                             reef = cbind(reef_matrix, 5),
                                             starting_values = starting_values,
                                             random = 0.25),
               regexp = "Please provide a 2-column with x,y coordinates of reef cells.")

})
