//#ifndef RCPP_MOVE_FISHPOP
//#define RCPP_MOVE_FISHPOP

void rcpp_move_behav(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_attr,
                     double move_mean, double move_sd, double move_reef,
                     double move_border, double move_return, double max_dist,
                     Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector extent,
                     Rcpp::IntegerVector dimensions);

//#endif // RCPP_MOVE_FISHPOP
