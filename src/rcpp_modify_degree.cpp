#include "rcpp_modify_degree.h"

//' rcpp_modify_degree
//'
//' @description Rcpp modify degree
//'
//' @param x Numeric with current angle in degree.
//' @param y Numerich with change of degree (negative or positive).
//'
//' @details
//' Rcpp implementation to substract or add degree to angle. Makes sure angles are
//' between 0 <= x <= 360.
//'
//' @return double
//'
//' @aliases rcpp_modify_degree
//' @rdname rcpp_modify_degree
//'
//' @export
// [[Rcpp::export]]
double rcpp_modify_degree(double x, double y) {

  // add value to degree
  x += y;

  // get reminder of division
  x = std::fmod(x, 360);

  // if x < 0, result will be negative
  if (x < 0) {

    x += 360;

  }

  return(x);
}
