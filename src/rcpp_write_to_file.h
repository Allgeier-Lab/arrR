//#ifndef RCPP_WRITE_TO_FILE
//#define RCPP_WRITE_TO_FILE

void rcpp_write_to_file(std::ofstream& file, Rcpp::NumericMatrix mat, int timestep, int burn_in);

//#endif // rcpp_write_to_file
