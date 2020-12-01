
parameters <- arrR::read_parameters("inst/extdata/parameters.csv", sep = ";")

starting_values <- arrR::read_parameters("inst/extdata/starting_values.csv", sep = ";")



test_that("input_seafloor creates RasterBrick", {

  input_seafloor <- arrR::setup_seafloor(extent = c(50, 50), grain = c(1, 1),
                                         reefs = reef_matrix,
                                         starting_values = starting_values,
                                         random = 0.0)
})
