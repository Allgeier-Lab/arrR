#include <Rcpp.h>
#include <iostream>
#include <fstream>

#include "rcpp_write_to_file.h"

using namespace Rcpp;

// [[Rcpp::interfaces(cpp)]]

// write_to_file
//
// @description
// Rcpp write matrix to file.
//
// @param file ofstream with file to write.
// @param mat Matrix to write.
// @param timestep Integer with current timestep.
//
// @details
// Writes all values in a text file. No R export because of ofstream.
//
// @return void
//
// @aliases rcpp_write_to_file
// @rdname rcpp_write_to_file

void rcpp_write_to_file(std::ofstream& file, Rcpp::NumericMatrix mat, int timestep, int burn_in) {

  std::string burn;

  if (timestep > burn_in) {

    burn = "no";

  } else {

    burn = "yes";

  }

  // loop through all rows
  for (int i = 0; i < mat.nrow(); i++) {

    // loop through all cols
    for (int j = 0; j < mat.ncol(); j++) {

      // write value
      file << mat(i, j) << " ";

    }

    // jump to next row in file
    file << timestep << " " << burn << std::endl;

  }
}
