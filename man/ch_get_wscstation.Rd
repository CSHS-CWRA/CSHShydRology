% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch_get_wscstation.R
\name{ch_get_wscstation}
\alias{ch_get_wscstation}
\title{Reads station information from a data file produced by ECDE}
\usage{
ch_get_wscstation(stnID, metadata = NULL)
}
\arguments{
\item{stnID}{A Water Survey of Canada station number}

\item{metadata}{a data frame of station information from ECDataExplorer. The data frame \option{HYDAT_list} is supplied with this package.}
}
\value{
Returns a line from a data frame with 21 variables
\item{Station}{StationID}
\item{StationName}{Station Name}
\item{HYDStatus}{Active or Discontinued}
\item{Prov}{Province}
\item{Latitude}{}
\item{Longitude}{}
\item{DrainageArea}{Area in km\eqn{^2}{^2}}
\item{Years}{# of years with data}
\item{From}{Start Year}
\item{To}{End Year}
\item{Reg.}{Regulated or natural}
\item{Flow}{if TRUE/Yes flow data is available}
\item{Level}{if TRUE/Yes water level data is available}
\item{Sed}{if TRUE/Yes sediment data is available}
\item{OperSched}{Current operation schedule- Continuous or Seasonal}
\item{RealTime}{if TRUE/Yes real itme data exists}
\item{RHBN}{if TRUE/Yes is in the reference hydrologic basin network}
\item{Region}{WSC Region}
\item{Datum}{Datum used}
\item{Operator}{Agency responsible for collecting data}
\item{Station_lname}{Added field combining StationID, StationName, Province and if station is RHBN an * is added}
}
\description{
Retrieves station information for an individual Water Survey of Canada site, based on stationID; 
adds a text string at position 21 that combines key elements for a title.
}
\examples{
data("HYDAT_list")
s_info <- ch_get_wscstation("05BB001", metadata = HYDAT_list)
title <- s_info[21]
print(title)

}
\author{
Paul Whitfield
}
