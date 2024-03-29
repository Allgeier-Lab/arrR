% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{rcpp_move_rand}
\alias{rcpp_move_rand}
\title{rcpp_move_rand}
\usage{
rcpp_move_rand(
  fishpop,
  move_mean,
  move_sd,
  max_dist,
  reef_attraction,
  coords_reef,
  extent,
  dimensions
)
}
\arguments{
\item{fishpop}{Matrix with fishpop values.}

\item{move_mean, move_sd}{Double with mean and variance movement parameter.}

\item{max_dist}{Numeric with maximum movement distance}

\item{reef_attraction}{Bool if attracted towards reef.}

\item{coords_reef}{Matrix with ID and coords of reef cells.}

\item{extent}{Vector with extent (xmin,xmax,ymin,ymax).}

\item{dimensions}{Vector with dimensions (nrow, ncol).}
}
\value{
void
}
\description{
Rcpp simulate movement (rand/attr).
}
\details{
Simulate movement of indivudals eiter either random (\code{reef_attraction = FALSE})
or attracted towards the reef cells (\code{reef_attraction = TRUE}).

In the case of random movement, each time step a random movement distance
is drawn from a lognorm distribution and the individal moves into a random heading
direction drawn from an uniform distribution.

In the case of attracted movement, fish individuals are aware of the distance to
the closest reef cell in three directions ahead of them (-45, 0, 45 degree) and
always move in the direction of the shortest distance to a reef cell.
}
\keyword{internal}
