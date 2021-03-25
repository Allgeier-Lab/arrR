# get parameters
parameters <- arrR::default_parameters

starting_values <- arrR::default_starting_values

# create reef
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
                      ncol = 2, byrow = TRUE)

# set extent
extent <- c(50, 50)

# set grain
grain <- c(1, 1)

# create input seafloor
input_seafloor <- arrR::setup_seafloor(extent = extent, grain = grain,
                                       reefs = reef_matrix,
                                       starting_values = starting_values,
                                       random = 0.0)

input_seafloor_rnd <- arrR::setup_seafloor(extent = extent, grain = grain,
                                           reefs = reef_matrix,
                                           starting_values = starting_values,
                                           random = 0.25)

test_that("setup_seafloor creates RasterBrick", {

  expect_s4_class(object = input_seafloor, class = "RasterBrick")

})

test_that("setup_seafloor has correct dimensions", {

  expect_equal(object = raster::ncell(input_seafloor),
               expected = extent[[1]] * extent[[2]])

  expect_equal(object = raster::res(input_seafloor),
               expected = grain)

})

test_that("setup_seafloor includes reef cells", {

  expect_equal(object = sum(raster::values(input_seafloor$reef)),
               expected = nrow(reef_matrix))

})

test_that("setup_seafloor has correct starting values", {

  expect_equal(object = mean(raster::values(input_seafloor$ag_biomass), na.rm = TRUE),
               expected = starting_values$ag_biomass)

  expect_equal(object = mean(raster::values(input_seafloor$bg_biomass), na.rm = TRUE),
               expected = starting_values$bg_biomass)

  expect_equal(object = mean(raster::values(input_seafloor$nutrients_pool), na.rm = TRUE),
               expected = starting_values$nutrients_pool)

  expect_equal(object = mean(raster::values(input_seafloor$detritus_pool), na.rm = TRUE),
               expected = starting_values$detritus_pool)

})

test_that("setup_seafloor adds random noise", {

  range_homo <- range(raster::values(input_seafloor$ag_biomass), na.rm = TRUE)

  range_random <- range(raster::values(input_seafloor_rnd$ag_biomass), na.rm = TRUE)

  expect_equal(object = range_homo[[1]],
               expected = range_homo[[2]])

  expect_lt(object = range_random[[1]],
            expected = range_random[[2]])

})

test_that("setup_seafloor returns error", {

  expect_error(object = arrR::setup_seafloor(extent = extent, grain = grain,
                                             reefs = 5,
                                             starting_values = starting_values,
                                             random = 0.25),
               regexp = "Please provide a 2-column with x,y coordinates of reef cells.")

  expect_error(object = arrR::setup_seafloor(extent = extent, grain = grain,
                                             reefs = cbind(reef_matrix, 5),
                                             starting_values = starting_values,
                                             random = 0.25),
               regexp = "Please provide a 2-column with x,y coordinates of reef cells.")

})
