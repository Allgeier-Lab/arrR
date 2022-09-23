//#ifndef RCPP_FISHPOP_GROWTH
//#define RCPP_FISHPOP_GROWTH

void rcpp_fishpop_growth(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                         Rcpp::NumericMatrix seafloor,
                         double pop_k, double pop_linf, double pop_a, double pop_b,
                         double pop_n_body, double pop_reserves_max, double pop_reserves_consump,
                         Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                         double min_per_i);

//#endif // RCPP_FISHPOP_GROWTH
