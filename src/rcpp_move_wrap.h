//#ifndef RCPP_MOVE_WRAP
//#define RCPP_MOVE_WRAP

void rcpp_move_wrap(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix coords_reef,
                    Rcpp::String movement, Rcpp::NumericVector pop_reserves_thres,
                    double move_mean, double move_var, double move_visibility,
                    double move_reef, double move_border, double move_return, double max_dist,
                    Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions);

//#endif // RCPP_MOVE_WRAP
