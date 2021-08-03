#include <Rcpp.h>
#include "rcpp_modify_degree.h"

using namespace Rcpp;

//' rcpp_modify_degree
//'
//' @description
//' Rcpp modify degree.
//'
//' @param x Numeric with current angle in degree.
//' @param y Numerich with change of degree (negative or positive).
//'
//' @details
//' Modify the degree of direction heading of individuals. The function ensures that
//' all degree are between 0 <= x <= 360.
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

/*** R
rcpp_modify_degree(x = 332, y = 33.5)
*/
