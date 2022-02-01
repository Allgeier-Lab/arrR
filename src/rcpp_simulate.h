//#ifndef RCPP_SIMULATE
//#define RCPP_SIMULATE

void rcpp_simulate(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix fishpop, Rcpp::NumericVector nutrients_input,
                   Rcpp::List seafloor_track, Rcpp::List fishpop_track, Rcpp::List parameters, Rcpp::String movement,
                   Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                   int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in,
                   bool verbose);

//#endif // RCPP_SIMULATE
