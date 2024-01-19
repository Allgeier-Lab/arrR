## code to prepare `DATASET` dataset goes here

default_parameters <- list(

  # belowground biomass
  bg_biomass_min = 275.89,
  bg_biomass_max = 1200,
  bg_v_max = 9.8,
  bg_k_m = 178.1,
  bg_gamma = 0.0082,

  # aboveground biomass
  ag_biomass_min = 8.87,
  ag_biomass_max = 500,
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
  move_mean = c(10.0, 10.0),
  move_sd = c(5.0, 5.0),
  move_reef = c(1.0, 1.0),
  move_return = c(15.0, 15.0),
  move_border = c(1.0, 1.0),

  # fishpop dimensions
  pop_a = c(0.0121, 0.0198),
  pop_b = c(3.161, 3),
  pop_k = c(0.2, 1.2),
  pop_linf = c(41.6, 26.5),
  pop_n_body = c(0.02999, 0.027518),

  # fishpop reserves
  pop_reserves_max = c(0.05, 0.05),
  pop_reserves_thres_mean = c(0.1, 0.1),
  pop_reserves_thres_sd = c(0.0, 0.0),
  pop_reserves_consump = c(0.1, 0.1),

  # fishpop respiration
  resp_intercept = c(0.0108, 0.0108), #jakes data
  resp_slope = c(-0.2, -0.2),
  resp_temp_low = c(2.1, 2.1),
  resp_temp_optm = c(36.0, 26.0),
  resp_temp_max = c(40.0, 40.0),
  temperature = 26.0,

  # fishpop behavior
  pop_behav = c(0, 1),
  pop_ldie = c(41.6, 26.5) # change according to desired constraints
)

ag <- default_parameters$ag_biomass_min +
  (default_parameters$ag_biomass_max - default_parameters$ag_biomass_min) * 0.5

bg <- default_parameters$bg_biomass_min +
  (default_parameters$bg_biomass_max - default_parameters$bg_biomass_min) * 0.5

default_starting <- list(

  # biomass
  bg_biomass = bg,
  ag_biomass = ag,

  # nutrients/detritus
  nutrients_pool = 0.25,
  detritus_pool = 0.25,

  # fishpop related
  pop_n = 8,
  pop_mean_size = c(9.0, 5.733),
  pop_sd_size = c(10.0, 6.37)
)

usethis::use_data(default_parameters, default_starting, overwrite = TRUE)
