#include <Rcpp.h>
#include "rcpp_convert_nutr.h"

using namespace Rcpp;

//' rcpp_convert_nutr
//'
//' @description
//' Rcpp convert nutrients.
//'
//' @param x Numeric with nutrient amount.
//' @param to String to specify in which unit to convert.
//'
//' @details
//' Convert nutrients between g and umol based on molecular mass of (ammonium; NH4)
//'
//' @references
//' https://en.wikipedia.org/wiki/Ammonium
//'
//' @return double
//'
//' @aliases rcpp_convert_nutr
//' @rdname rcpp_convert_nutr
//'
//' @export
// [[Rcpp::export]]
double rcpp_convert_nutr(double x, Rcpp::String to) {

  // convert to gram by multiplying factor
  if (to == "g") {

    double result = x * 18.039 / std::pow(10, 6);

    return(result);

  // convert to umol by multiplying factor
  } else if (to == "umol") {

    double result = x * std::pow(10, 6) / 18.039;

    return(result);

  // throw error if to option is not present
  } else {

    Rcpp::stop("Please select either to='g' or to='umol'.");

  }
}

/*** R
rcpp_convert_nutr(x = 0.005, to = "umol")
*/
