# arrR 0.2.5.1
* Renamed all pop_\*_grunt parameters to only pop_\*
* Adding burn-in option for run_simulation

# arrR 0.2.5
* Renamed filter_result to filter_mdlrn
* Slight changes to print.mdl_rn
* Rename summarize_result to summarize_mdlrn and allow to select summary fun

# arrR 0.2.4
* Better memory usage by using mainly Rcpp for core functions
* Removed pop_max_size and replace with pop_linf_grunt

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
* Including Rcpp

# arrR 0.1.0

* First re-implementation of model code
