% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_seafloor_dim.R
\name{get_seafloor_dim}
\alias{get_seafloor_dim}
\title{get_seafloor_dim}
\usage{
get_seafloor_dim(seafloor)
}
\arguments{
\item{seafloor}{Data.frame with seafloor values}
}
\value{
list
}
\description{
Setup seafloor for model run.
}
\details{
Returns dimensions, extent, and grain of seafloor data.frame.
}
\examples{
\dontrun{
reef <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
ncol = 2, byrow = TRUE)

seafloor <- setup_seafloor(dimensions = c(50, 50), grain = 1,
reef = reef, starting_values = default_starting)

get_seafloor_dim(seafloor = seafloor)
}

}
\keyword{internal}
