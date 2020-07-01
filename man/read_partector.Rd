% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_partector.R
\name{read_partector}
\alias{read_partector}
\title{Title}
\usage{
read_partector(path, tz = "America/New_York", metadata = FALSE)
}
\arguments{
\item{path}{path of Partector output file.}

\item{tz}{a character string that specifies which time zone to parse the
datetime. Default = 'America/New_York.'}

\item{metadata}{include Partector output data in addition to LDSA, particle
number, and mass. Default = FALSE.}
}
\value{
a tibble.
}
\description{
Title
}
\examples{
#'
\dontrun{

read_partector(path, tz = "America/New_York", metadata = FALSE)
}
}