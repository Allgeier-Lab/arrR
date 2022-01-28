//#ifndef RCPP_SEAGRASS_GROWTH
//#define RCPP_SEAGRASS_GROWTH

void rcpp_seagrass_growth(Rcpp::NumericMatrix seafloor,
                          double bg_v_max, double bg_k_m, double bg_gamma,
                          double ag_v_max, double ag_k_m, double ag_gamma,
                          double bg_biomass_max, double bg_biomass_min,
                          double ag_biomass_max, double ag_biomass_min,
                          double seagrass_thres, double seagrass_slope,
                          double seagrass_slough, double time_frac);

//#endif // RCPP_SEAGRASS_GROWTH
