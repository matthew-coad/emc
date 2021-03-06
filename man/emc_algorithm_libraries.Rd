% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithms.R
\name{emc_algorithm_libraries}
\alias{emc_algorithm_libraries}
\title{Lists libraries required by emc algorithms}
\usage{
emc_algorithm_libraries(algorithms, uninstalled_only = TRUE)
}
\arguments{
\item{algorithms}{Input algorithms as returned by /code{emc_algorithms}}

\item{uninstalled_only}{If TRUE only uninstalled libraries are returned.}
}
\value{
Vector of library names
}
\description{
Given a number of algorithms given as a tibble, returns all the libraries that need to be
installed to use those algorithms.
}
