# arrR 0.2.9
* Fix bug that not all starting values are included in `mdl_rn` object

# arrR 0.2.8
* Add hexlogo
* Add `extract_result` function and `extract` argument to `run_simulation`
* Add `bg_gamma` and `ag_gamma` to parameters. Was hard-coded previously
* Better checking if object is `mdl_rn`
* Renamed master branch to main

# arrR 0.2.7
* Add default parameters and starting values as data
* Add structure for vignettes
* Use absolute iterations for burn-in
* Add `return_burnin` argument to `run_simulation`

# arrR 0.2.6
* Renamed all pop_\*_grunt parameters to only pop_\*
* Adding burn-in option to run_simulation
* Track production, slough, nutrient uptake cumulative for each cell
* `summarize_mdlrn` returns `burn_in` column

# arrR 0.2.5
* Renamed filter_result to `filter_mdlrn`
* Slight changes to `print.mdl_rn`
* Rename `summarize_result` to `summarize_mdlrn` and allow to select summary fun

# arrR 0.2.4
* Better memory usage by using mainly Rcpp for core functions
* Removed `pop_max_size` and replace with `pop_linf_grunt`

# arrR 0.2.3
* Add GitHub actions
* Resulting object contains parameters and starting values
* `get_limits` can return limits over time period
* `plot.mdl_rn` also takes limits if `summarize = TRUE` for seafloor

# arrR 0.2.2
* Better plotting labels
* Renamed `parameters$detritus_decomposition` to `parameter$detritus_mineralization`

# arrR 0.2.1
* Reworked seagrass growth
* Rename package to `arrR`
* Add `pkgdown` homepage

# arrR 0.2.0
* Speed and RAM improvements of many function
* Including `Rcpp`

# arrR 0.1.0
* First re-implementation of model code
