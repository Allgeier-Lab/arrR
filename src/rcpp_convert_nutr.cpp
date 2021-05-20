#include "rcpp_convert_nutr.h"

//' rcpp_convert_nutr
//'
//' @description Rcpp convert nutrients
//'
//' @param x Numeric with nutrient amount.
//' @param to String to specify in which unit to convert.
//'
//' @details
//' Rcpp implementation to convert nutrients between g and umol.
//'
//' @return double
//'
//' @aliases rcpp_convert_nutr
//' @rdname rcpp_convert_nutr
//'
//' @export
// [[Rcpp::export]]
double rcpp_convert_nutr(double x, String to) {

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

    throw std::range_error("Please select either to='g' or to='umol'.");

  }
}
