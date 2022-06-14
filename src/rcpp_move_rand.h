//#ifndef RCPP_MOVE_RAND
//#define RCPP_MOVE_RAND

void rcpp_move_rand(Rcpp::NumericMatrix fishpop, Rcpp::NumericVector move_mean,
                    Rcpp::NumericVector move_sd, Rcpp::NumericVector max_dist, bool reef_attraction,
                    Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector extent,
                    Rcpp::IntegerVector dimensions);

//#endif // RCPP_MOVE_RAND
