//#ifndef RCPP_MORTALITY
//#define RCPP_MORTALITY

void rcpp_mortality(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                    Rcpp::NumericMatrix seafloor,
                    double pop_ldie, double pop_n_body, double pop_reserves_max,
                    Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions);

//#endif // RCPP_MORTALITY
