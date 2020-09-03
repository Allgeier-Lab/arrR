
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coRal

<!-- badges: start -->

<!-- badges: end -->

The goal of coRal is to …

## Installation

You can install coRal from
[GitHub](https://github.com/Allgeier-Lab/coRal) with the following line
of code. You will need the `auth_token` because the repository is
currently set to private so only members of the Allgeier Lab can see it.
**PLEASE DON’T SHARE THIS TOKEN WITH ANYONE WITHOUT LETTING ME KNOW
FIRST**.

``` r
remotes::install_github("Allgeier-Lab/coRAL", 
                        auth_token = "e46c8683663fd7a14869c949a48582063e64b915")
```

<!-- Add CRAN link if applicable -->

## Example

To access all functions to run the mode, simply load the library.

``` r
library(coRal)
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
starting_values <- system.file("extdata", "starting_values.csv", package = "coRal")
parameters <- system.file("extdata", "parameters.csv", package = "coRal")

starting_values <- read_parameters(file = starting_values, sep = ";")
parameters <- read_parameters(file = parameters, sep = ";")

check_parameters(starting_values = starting_values, parameters = parameters)
#> > All starting values and parameters are available...
```

To setup the simulation seafloor and individuals, simply run
`setup_seafloor` and `setup_population`. If you want to add artificial
reefs to the seafloor, provide a `matrix` with x,y coordinates of all AR
cells.

``` r
reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0), 
                      ncol = 2, byrow = TRUE)

input_seafloor <- setup_seafloor(extent = c(50, 50), grain = 1, reefs = reef_matrix, 
                                       starting_values = starting_values, parameters = parameters)
#> > Creating seafloor with extent(50, 50)...
#> > Creating 5 artifical reef cells...

input_fish_population <- setup_fish_population(seafloor = input_seafloor, 
                                               starting_values = starting_values, 
                                               parameters = parameters)
#> > Creating 10 individuals within extent(-25, 25, -25, 25)...
```

To rum a simulation, simply provide the previously created seafloor and
population as well as all parameters and starting values the
`run_simulation` function. Additionally, you need to specify the number
of time steps that are simulated.

``` r
result <- run_simulation(seafloor = input_seafloor, 
                         fish_population = input_fish_population, 
                         starting_values = starting_values, parameters = parameters, 
                         reef_attraction = TRUE,
                         max_i = 10800, min_per_i = 120,
                         verbose = FALSE)

result
#> Total simulated time: 900 d 
#> 
#> Seafloor: (ag_biomass, bg_biomass, detritus_pool, detritus_dead, wc_nutrients)
#> Minimum: 0, 0, 0, 0, -1e-05
#> Median: 2270.65862, 2657.55888, 1.9981, 0, 0.00016
#> Maximum: 2270.65862, 2657.55888, 2.00419, 19.55918, 0.01807
#> 
#> Fish population: (length, weight)
#> Minimum: 4.469, 1.37436
#> Median: 7.48477, 7.01894
#> Maximum: 14.42224, 55.78125
#> Total deaths: 10
```

To plot the results, pass the resulting object to the `plot` function.
This will automatically create a plot of the selected timestep (default:
last timestep). It is possible to either plot a single `RasterLayer` or
the whole `RasterBrick`

``` r

plot(result, fill = "detritus_pool")
```

<img src="man/figures/README-plot-1.png" width="100%" style="display: block; margin: auto;" />

<!-- The animate the results, pass the resulting object to `animate_result`. This will produce a GIF, but you need the packages `ggplot2`, `gganimate` and `gifski`. -->

<!-- ```{r animate_sim, fig.align="center"} -->

<!-- animate_result(result, fill = "detritus_pool", end_pause = 10) -->

<!-- ```  -->

### Code of Conduct

Please note that the coRal project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
