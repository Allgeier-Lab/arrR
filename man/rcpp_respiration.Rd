% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{rcpp_respiration}
\alias{rcpp_respiration}
\title{rcpp_respiration}
\usage{
rcpp_respiration(
  fishpop,
  resp_intercept,
  resp_slope,
  resp_temp_low,
  resp_temp_max,
  resp_temp_optm,
  water_temp,
  min_per_i
)
}
\arguments{
\item{fishpop}{Matrix with fishpop values.}

\item{resp_intercept, resp_slope}{Numeric with regression parameters.}

\item{resp_temp_low, resp_temp_max, resp_temp_optm}{Numeric with water temperature parameters.}

\item{water_temp, min_per_i}{Numeric with various parameters.}
}
\value{
void
}
\description{
Rcpp simulate respration.
}
\details{
Function to simulate respiration of fish individuals based on movement, body +
size and water temperature. The respiration is temperature dependent with an
activity multiplier (Hanson et al. 1997). Originally descibed in Kitchell et al. (1977).

If respiration is a infinite number (due to zero division), set to
\code{erespiration=1.0}.
}
\references{
Hanson, P.C., Johnson, T.B., Schindler, D.E., Kitchell, J.F., 1997. Fish
Bioenergetics 3.0 for Windows manual (Manual). University of Wisconsin-Madison,
Centre for Limnology, Madison,USA.

Kitchell, J.F., Stewart, D.J., Weininger, D., 1977. Applications of a bioenergetics
model to Yellow Perch (Perca flavescens) and Walleye (Stizostedion vitreum vitreum).
J. Fish. Res. Bd. Can. 34, 1922–1935. <https://doi.org/10.1139/f77-258>
}
\keyword{internal}
