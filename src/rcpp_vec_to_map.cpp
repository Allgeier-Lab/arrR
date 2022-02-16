#include <Rcpp.h>
#include <map>

#include "rcpp_vec_to_map.h"

using namespace Rcpp;

// rcpp_vec_to_map
//
// @description
// Rcpp Vector to map.
//
// @param key,value NumericVector with key and values values
//
// @details
// Converts two vectors to a \code{std::map} object.
//
// @return map
//
// @aliases rcpp_vec_to_map
// @rdname rcpp_vec_to_map
//
// @keywords internal
std::map<int, double> rcpp_vec_to_map(Rcpp::NumericVector key, Rcpp::NumericVector value) {

  if (key.length() != value.length()) Rcpp::stop("'key' and 'value' must be same length.");

  // init map
  std::map<int, double> map_mat;

  // loop through all keys
  for(int i = 0; i < key.length(); i++) {

    // insert key and value into map
    map_mat.insert(std::make_pair(key(i), value(i)));

  }

  return map_mat;
}

