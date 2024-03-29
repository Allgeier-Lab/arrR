//#ifndef RCPP_MOVE_RAND
//#define RCPP_MOVE_RAND

void rcpp_move_rand(Rcpp::NumericMatrix fishpop, double move_mean, double move_sd,
                    double max_dist, bool reef_attraction, Rcpp::NumericMatrix coords_reef,
                    Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions);

//#endif // RCPP_MOVE_RAND
