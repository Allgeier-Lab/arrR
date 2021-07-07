
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *arrR*

<!-- badges: start -->

[![Project
Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R build
status](https://github.com/Allgeier-Lab/arrR/workflows/R-CMD-check/badge.svg)](https://github.com/Allgeier-Lab/arrR/actions)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

<img src="man/figures/logo.png" align="right" width="150" />

The goal of *arrR* is to simulate seagrass growth around artificial
reefs.

## Installation

You can install *arrR* from
[GitHub](https://github.com/Allgeier-Lab/arrR) with the following line
of code. You will need the `auth_token` because the repository is
currently set to private so only members of the Allgeier Lab can see it.
**PLEASE DON’T SHARE THIS TOKEN WITH ANYONE WITHOUT LETTING ME KNOW
FIRST**.

``` r
remotes::install_github(repo = "Allgeier-Lab/arrR",  ref = "development",
                        auth_token = "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")
```

<!-- Add CRAN link if applicable -->

## Example

To access all functions to run the mode, simply load the library.

``` r
library(arrR)
```

The starting values and parameters must be imported as two separated
list with a named object for each value. There is a function to import
these values and automatically convert them to a list
(`read_parameters`). The files must be a table with two columns. The
first column must be called “parameter” and include a string of all
parameter names. The second column must the called “value” and include
the actual values.

To check if all parameters are available, use `check_parameters`.

``` r
starting_values <- arrR::default_starting_values

parameters <- arrR::default_parameters

check_parameters(starting_values = starting_values, parameters = parameters)
#> > ...Checking starting values...
#> > ...Checking parameter values...
#> > ...Checking if starting values are within parameter boundaries...
#> > All checking done!
```

To setup the simulation seafloor and individuals, simply run
`setup_seafloor` and `setup_population`. If you want to add artificial
reefs to the seafloor, provide a `matrix` with x,y coordinates of all AR
cells.

``` r
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0), 
                      ncol = 2, byrow = TRUE)

input_seafloor <- setup_seafloor(extent = c(50, 50), grain = 1, 
                                 reefs = reef_matrix, 
                                 starting_values = starting_values)
#> > ...Creating seafloor with extent(50, 50)...
#> > ...Creating 5 artifical reef cells...

input_fishpop <- setup_fishpop(seafloor = input_seafloor, 
                               starting_values = starting_values, 
                               parameters = parameters)
#> > ...Creating 8 individuals within extent(-25, 25, -25, 25)...
```

To run a simulation, simply provide the previously created seafloor and
population as well as all parameters to the `run_simulation` function.
Additionally, you need to specify the number of timesteps that are
simulated. Check out `?run_simulation` to see further possible
specifications of the model run.

``` r
min_per_i <- 120

# run the model for three years
years <- 3
max_i <- (60 * 24 * 365 * years) / min_per_i

# save results only every 15 days
days <- 5
save_each <- (24 / (min_per_i / 60)) * days

result <- run_simulation(seafloor = input_seafloor, 
                         fishpop = input_fishpop,
                         parameters = parameters, 
                         movement = "rand",
                         max_i = max_i, min_per_i = min_per_i, 
                         save_each = save_each)

result
#> Total time : 13140 iterations (1095 days) [Burn-in: 0 iter.]
#> Saved each : 60 iterations (5 days)
#> Seafloor   : extent(-25, 25, -25, 25), 5 reef cells
#> Fishpop    : 8 indiv (movement: 'rand')
#> 
#> Seafloor : (ag_biomass, bg_biomass, nutrients_pool, detritus_pool, detritus_fish)
#> Minimum  : 62.14, 535.131, 0, 1.524, 0
#> Mean     : 62.252, 541.715, 0, 1.535, 0
#> Maximum  : 65.061, 622.339, 0.006, 1.539, 0
#> 
#> Fishpop  : (length, weight, died_consumption, died_background)
#> Minimum  : 20.314, 164.723, 0, 0
#> Mean     : 22.991, 247.922, 0, 0
#> Maximum  : 26.127, 364.934, 0, 0
```

To plot the results, pass the resulting object to the `plot` function.
This will automatically create a plot of the selected timestep (default:
final timestep). Plotting methods are available for both the seafloor
and the fish population using the `what` argument.

``` r
plot(result, what = "seafloor")
```

<img src="man/figures/README-plot-1.png" width="100%" style="display: block; margin: auto;" />

### Code of Conduct

Please note that the *arrR* project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
