//#ifndef RCPP_FISHPOP_GROWTH
//#define RCPP_FISHPOP_GROWTH

void rcpp_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                         Rcpp::NumericMatrix seafloor,
                         Rcpp::NumericVector pop_k, Rcpp::NumericVector pop_linf,
                         Rcpp::NumericVector pop_a, Rcpp::NumericVector pop_b,
                         Rcpp::NumericVector pop_n_body, Rcpp::NumericVector pop_reserves_max,
                         Rcpp::NumericVector pop_reserves_consumption,
                         Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                         double min_per_i);

//#endif // RCPP_FISHPOP_GROWTH
