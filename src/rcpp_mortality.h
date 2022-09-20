//#ifndef RCPP_MORTALITY
//#define RCPP_MORTALITY

void rcpp_mortality(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                    Rcpp::NumericMatrix seafloor,
                    double pop_mean_size, double pop_n_body, double pop_linf, double pop_reserves_max,
                    Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions);

//#endif // RCPP_MORTALITY
