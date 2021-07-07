## code to prepare `DATASET` dataset goes here

default_parameters <- list(

  ag_biomass_max = 193.01,
  ag_biomass_min = 8.87,
  ag_v_max = 8.1,
  ag_k_m = 12.6,
  ag_gamma = 0.0144,

  bg_biomass_max = 933.03,
  bg_biomass_min = 275.89,
  bg_v_max = 9.8,
  bg_k_m = 178.1,
  bg_gamma = 0.0082,

  seagrass_thres = 1/2,
  seagrass_slope = 2.0,
  seagrass_slough = 0.0001,

  nutrients_diffusion = 2/3,
  nutrients_output = 0.0,

  detritus_mineralization = 0.0001,
  detritus_diffusion = 1/3,
  detritus_fish_decomp = 0.5,
  detritus_fish_diffusion = 1/3,

  move_mean = 10.0,
  move_var = 5.0,
  move_visibility = 1.0,
  move_border = 2.0,
  move_reef = 0.5,
  move_return = 50.0,

  pop_a = 0.0121,
  pop_b = 3.161,
  pop_k = 0.2,
  pop_linf = 41.6,
  pop_n_body = 0.02999,

  pop_reserves_max = 0.5,
  pop_reserves_thres_lo = 0.5,
  pop_reserves_thres_hi = 0.5,
  pop_reserves_consump = 1.0,

  resp_intercept = 0.0108,
  resp_slope = -0.2,
  resp_temp_low = 2.1,
  resp_temp_optm = 36,
  resp_temp_max = 40
)

ag <- default_parameters$ag_biomass_min +
  (default_parameters$ag_biomass_max - default_parameters$ag_biomass_min) * 0.5

bg <- default_parameters$bg_biomass_min +
  (default_parameters$bg_biomass_max - default_parameters$bg_biomass_min) * 0.5

default_starting_values <- list(

  ag_biomass = ag,
  bg_biomass = bg,

  nutrients_pool = 0.25,
  detritus_pool = 0.25,

  pop_n = 8,
  pop_mean_size = 9.0,
  pop_var_size = 10.0
)

usethis::use_data(default_parameters, default_starting_values, overwrite = TRUE)
