//#ifndef RCPP_MOVE_WRAP
//#define RCPP_MOVE_WRAP

void rcpp_move_wrap(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix pop_reserves_thres,
                    std::string movement, Rcpp::NumericVector move_mean, Rcpp::NumericVector move_sd, Rcpp::NumericVector move_reef,
                    Rcpp::NumericVector move_border, Rcpp::NumericVector move_return, Rcpp::NumericVector max_dist,
                    Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector extent,
                    Rcpp::IntegerVector dimensions, Rcpp::NumericVector behavior);

//#endif // RCPP_MOVE_WRAP
