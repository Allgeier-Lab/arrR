//#ifndef RCPP_REINCARNATE
//#define RCPP_REINCARNATE

void rcpp_reincarnate(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_track, int fish_id,
                      Rcpp::NumericMatrix seafloor, Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                      double pop_n_body, std::string reason);

//#endif // RCPP_REINCARNATE
