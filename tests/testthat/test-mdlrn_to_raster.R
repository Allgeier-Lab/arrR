# convert to mdl_rn
seafloor_ras <- arrR::mdlrn_to_raster(mdl_rn = result_rand)

# get cell values
seafloor_max_i <- dplyr::filter(result_rand$seafloor, timestep == max_i)

test_that("mdlrn_to_raster returns raster", {

  expect_s4_class(object = seafloor_ras, class = "RasterBrick")

})

test_that("mdlrn_to_raster returns correct dimensions", {

  expect_equal(object = raster::res(seafloor_ras), expected = result_rand$grain)

  expect_equal(object = raster::extent(seafloor_ras), expected = result_rand$extent)

})

test_that("mdlrn_to_raster returns correct values", {

  expect_equal(object = seafloor_ras$ag_biomass[], expected = seafloor_max_i$ag_biomass)

  expect_equal(object = seafloor_ras$bg_biomass[], expected = seafloor_max_i$bg_biomass)

  expect_equal(object = seafloor_ras$nutrients_pool[], expected = seafloor_max_i$nutrients_pool)

  expect_equal(object = seafloor_ras$detritus_pool[], expected = seafloor_max_i$detritus_pool)

})

test_that("mdlrn_to_raster returns error", {

  expect_error(object = arrR::mdlrn_to_raster(mdl_rn = list(c(1:5), c(5:10))),
               regexp = "Please provide 'mdl_rn' object.")

})
