//#ifndef RCPP_SIM_PROCESSES
//#define RCPP_SIM_PROCESSES

void rcpp_sim_processes(Rcpp::NumericMatrix seafloor, Rcpp::NumericMatrix fishpop,
                        Rcpp::List seafloor_track, Rcpp::List fishpop_track, Rcpp::List parameters,
                        int pop_n, Rcpp::String movement, double max_dist, Rcpp::NumericVector pop_reserves_thres,
                        Rcpp::NumericMatrix coords_reef, Rcpp::NumericMatrix cell_adj,
                        Rcpp::NumericVector extent, Rcpp::IntegerVector dimensions,
                        Rcpp::NumericVector nutrients_input,
                        int max_i, int min_per_i, int save_each, int seagrass_each, int burn_in,
                        bool verbose);

//#endif // RCPP_SIM_PROCESSES
