//#ifndef RCPP_MORTALITY
//#define RCPP_MORTALITY

void rcpp_mortality(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track,
                    Rcpp::NumericMatrix seafloor,
                    Rcpp::NumericVector pop_linf,  Rcpp::NumericVector pop_n_body,
                    Rcpp::NumericVector pop_reserves_max,
                    Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions);

//#endif // RCPP_MORTALITY
