% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_parameters.R
\name{read_parameters}
\alias{read_parameters}
\title{read_parameters}
\usage{
read_parameters(file, sep = ";", return_list = TRUE, ...)
}
\arguments{
\item{file}{String with path to text file.}

\item{sep}{String with separator of columns.}

\item{return_list}{Logical if TRUE, parameters are returned as list.}

\item{...}{Arguments passed on to \code{\link{read.table}}.}
}
\value{
list
}
\description{
Read parameters from text file.
}
\details{
Creates a list with all parameters or starting values or parameters. Columns
must be named 'parameter' and value'.
}
\examples{
\dontrun{
read_parameters(file = "starting-values.csv", sep = ";")
}

}
