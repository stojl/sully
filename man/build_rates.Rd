% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intensity_builders.R
\name{build_rates}
\alias{build_rates}
\title{build_rates}
\usage{
build_rates(x, type = "mpp")
}
\arguments{
\item{x}{matrix}

\item{type}{If you intend to use rates for rmarkov or rsemimarkov you must
specify which one. Either 'markov' or 'semimarkov'.}
}
\value{
list of functions
}
\description{
Constructs rates from a matrix of intensities to be used with rmpp.
}
