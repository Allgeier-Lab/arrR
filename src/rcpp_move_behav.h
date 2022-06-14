//#ifndef RCPP_MOVE_FISHPOP
//#define RCPP_MOVE_FISHPOP

void rcpp_move_behav(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix pop_reserves_thres,
                     Rcpp::NumericVector move_mean, Rcpp::NumericVector move_sd, Rcpp::NumericVector move_reef,
                     Rcpp::NumericVector move_border, Rcpp::NumericVector move_return, Rcpp::NumericVector max_dist,
                     Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector extent,
                     Rcpp::IntegerVector dimensions);

//#endif // RCPP_MOVE_FISHPOP
