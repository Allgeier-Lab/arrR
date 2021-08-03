//#ifndef RCPP_MOVE_RAND
//#define RCPP_MOVE_RAND

void rcpp_move_rand(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                    double move_mean, double move_var, double max_dist, bool reef_attraction,
                    Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions);

//#endif // RCPP_MOVE_RAND
