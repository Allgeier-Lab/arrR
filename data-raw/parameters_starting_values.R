## code to prepare `DATASET` dataset goes here

arrR_parameters <- list(

  # belowground biomass
  bg_biomass_min = 275.89,
  bg_biomass_max = 933.03,
  bg_v_max = 9.8,
  bg_k_m = 178.1,
  bg_gamma = 0.0082,

  # aboveground biomass
  ag_biomass_min = 8.87,
  ag_biomass_max = 193.01,
  ag_v_max = 8.1,
  ag_k_m = 12.6,
  ag_gamma = 0.0144,

  # seagrass related
  seagrass_thres = -1/3,
  seagrass_slope = 2.0,
  seagrass_slough = 0.001,

  # nutrients
  nutrients_diffusion = 2/3,
  nutrients_loss = 0.0,

  # detritus
  detritus_mineralization = 0.001,
  detritus_diffusion = 1/3,
  detritus_fish_decomp = 0.5,
  detritus_fish_diffusion = 1/3,
  detritus_loss = 0.0,

  # fishpop movement
  move_mean = 10.0,
  move_var = 5.0,
  move_reef = 1.0,
  move_return = 15.0,
  move_border = 1.0,

  # fishpop dimensions
  pop_a = 0.0121,
  pop_b = 3.161,
  pop_k = 0.2,
  pop_linf = 41.6,
  pop_n_body = 0.02999,

  # fishpop reserves
  pop_reserves_max = 0.05,
  pop_reserves_thres_mean = 0.1,
  pop_reserves_thres_var = 0.0,
  pop_reserves_consump = 0.1,

  # fishpop respiration
  resp_intercept = 0.0108,
  resp_slope = -0.2,
  resp_temp_low = 2.1,
  resp_temp_optm = 36,
  resp_temp_max = 40
)

ag <- arrR_parameters$ag_biomass_min +
  (arrR_parameters$ag_biomass_max - arrR_parameters$ag_biomass_min) * 0.5

bg <- arrR_parameters$bg_biomass_min +
  (arrR_parameters$bg_biomass_max - arrR_parameters$bg_biomass_min) * 0.5

arrR_starting_values <- list(

  # biomass
  bg_biomass = bg,
  ag_biomass = ag,

  # nutrients/detritus
  nutrients_pool = 0.25,
  detritus_pool = 0.25,

  # fishpop related
  pop_n = 8,
  pop_mean_size = 9.0,
  pop_var_size = 10.0
)

usethis::use_data(arrR_parameters, arrR_starting_values, overwrite = TRUE)
