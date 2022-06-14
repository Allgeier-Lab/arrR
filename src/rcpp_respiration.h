//#ifndef RCPP_RESPIRATION
//#define RCPP_RESPIRATION

void rcpp_respiration(Rcpp::NumericMatrix fishpop,
                      Rcpp::NumericVector resp_intercept, Rcpp::NumericVector resp_slope,
                      Rcpp::NumericVector resp_temp_low, Rcpp::NumericVector resp_temp_max,
                      Rcpp::NumericVector resp_temp_optm, double water_temp, double min_per_i);

//#endif // RCPP_RESPIRATION
