## code to prepare `DATASET` dataset goes here


default_parameters <- list(

  ag_biomass_max = 159.5,
  ag_biomass_min = 5.1,
  ag_v_max = 8.1,
  ag_k_m = 12.6,
  ag_gamma = 0.0144,

  bg_biomass_max = 746.9,
  bg_biomass_min = 122.1,
  bg_v_max = 9.8,
  bg_k_m = 178.1,
  bg_gamma = 0.0082,

  seagrass_thres = 1/2,
  seagrass_slope = 3,

  nutrients_diffusion = 0.6,
  nutrients_output = 0.0,

  detritus_ratio = 0.0001,
  detritus_mineralization = 0.0001,
  detritus_fish_ratio = 0.6,
  detritus_diffusion = 0.3,
  detritus_fish_diffusion = 0.3,

  pop_max_reserves = 0.05,
  pop_want_reserves = 0.01,

  pop_visibility = 1,
  pop_mean_move =  8,
  pop_var_move = 5,

  pop_a = 0.0121,
  pop_b = 3.161,
  pop_k = 0.2,
  pop_linf = 41.6,

  pop_n_body = 0.02999,

  resp_intercept = 0.0108,
  resp_slope = -0.2,
  resp_temp_low = 2.1,
  resp_temp_optm = 36,
  resp_temp_max = 40
)

default_starting_values <- list(

  ag_biomass = 6.644,
  bg_biomass =  128.348,

  nutrients_pool = 0.75,
  detritus_pool = 0.75,

  pop_n = 25,
  pop_mean_size = 9,
  pop_var_size = 10,

  water_temp = 26
)

usethis::use_data(default_parameters, default_starting_values, overwrite = TRUE)
