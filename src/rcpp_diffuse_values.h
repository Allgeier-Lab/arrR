//#ifndef RCPP_DIFFUSE_VALUES
//#define RCPP_DIFFUSE_VALUES

void rcpp_diffuse_values(Rcpp::NumericMatrix seafloor, Rcpp::IntegerMatrix cell_adj,
                         double nutrients_diffusion, double detritus_diffusion,
                         double detritus_fish_diffusion);

//#endif // RCPP_DIFFUSE_VALUES
