//#ifndef RCPP_MOVE_WRAP
//#define RCPP_MOVE_WRAP

void rcpp_move_wrap(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix pop_reserves_thres,
                    std::string movement, double move_mean, double move_sd, double move_reef,
                    double move_border, double move_return, double max_dist,
                    Rcpp::NumericMatrix coords_reef, Rcpp::NumericVector extent,
                    Rcpp::IntegerVector dimensions);

//#endif // RCPP_MOVE_WRAP
