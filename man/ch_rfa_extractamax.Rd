% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch_rfa_extractamax.R
\name{ch_rfa_extractamax}
\alias{ch_rfa_extractamax}
\alias{ch_rfa_extractamax.formula}
\alias{ch_rfa_extractamax.default}
\title{Extracts the annual maxima of a daily time series}
\usage{
ch_rfa_extractamax(x, ...)

\method{ch_rfa_extractamax}{formula}(form, x, tol = 0, ...)

\method{ch_rfa_extractamax}{default}(x, tol = 0, nlab = "n", ylab = "yy", ...)
}
\arguments{
\item{x}{Data. If no formula is passed, the first column must be the
value and the second the date.}

\item{...}{Other parameters.}

\item{form}{Formula of the form \code{value ~ date} that specifies the
variable from which the annual maximums are extracted and a date variable.}

\item{tol}{Filter the years having less than \code{tol} days.}

\item{nlab, ylab}{Names for the added columns representing respectively 
the number of yearly observations and the year. 
If set to \code{NULL} the given column is not added.}
}
\value{
Returns a data frame containing the annual (Monthly) maxima,
the date and the number of observations during the year.
}
\description{
Extracts the annual maxima of a daily time series
}
\examples{

out <- ch_rfa_extractamax(flow ~ date, CAN01AD002, tol = 350)
head(out)

}
\author{
Martin Durocher
}
