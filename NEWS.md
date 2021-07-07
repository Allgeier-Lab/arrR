# arrR 0.5.1
* Main simulation loop completely in Rcpp `rcpp_run_simulation`
* Better tracking of fish consumption and excretion
* Age counter in iterations
* Better name for `pop_reserves_*` parameters
* Renamed `setup_seafloor_mdlrn` to `mdlrn_to_raster`

# arrR 0.5.0
* Simpler allocation rule based on curves.
* Adding new movement behaviors code based on reserve
* Minor bugfixing and rename of many functions
* New `plot_threshold` function. 
* Fix bug in movement and specify maximum movement distance based on 95% of distribution
* Remove `rlognorm` again...lol
* Remove `extract_result` again
* Adding `rcpp_shuffle`
* Remove `reef_dist`
* Adding `rcpp_update_coords`
* Adding `rcpp_allocation_ratio` and smoother sigmoid function
* Adding `get_stable_values` function
* Calling all `rcpp_` functions directly if possible in `run_simulation`
* Renamed `detritus_ratio` to `seagrass_slough`
* Renamed `detritus_fish_ratio` to `detritus_fish_decomp`
* Remove `pop_want_reserves` parameter. Fish always try to fill up reserves

# arrR 0.4.0
* Add `simulate_input` and `simulate_output`
* Rename `simulate_fishpop_growth` to `simulate_growth`

# arrR 0.3.1
* Add `rlognorm` for better code structure

# arrR 0.3.0
* Update authors list
* Re-implement movement of fish individuals in Rcpp mainly
* Rename dead detritus to fish detritus (and all corresponding parameters)

# arrR 0.2.9
* Fix bug that not all starting values are included in `mdl_rn` object
* Print progress each timestep because why not?
  * Change to GPL3 license
* Fix bug in `rcpp_calc_seagrass_growth.cpp` with allocation rules

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
