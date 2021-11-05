#' List of Water Survey of Canada hydrometic stations.
#' 
#' @description A dataframe of station information, as extracted from HYDAT using ECDataExplorer.
#' 
#' @format A dateframe with a row for each station and 20 columns.
#' @source Water Survey of Canada
#' @details 
#' Variables: 
#' \describe{
#' \item{Station}{StationID}
#' \item{StationName}{Station Name}
#' \item{HYDStatus}{Active or Discontinued}
#' \item{Prov}{Province}
#' \item{Latitude}{}
#' \item{Longitude}{}
#' \item{DrainageArea}{km\eqn{^2}{^2}}
#' \item{Years}{Number of years with data}
#' \item{From}{Start Year}
#' \item{To}{End Year}
#' \item{Reg.}{Regulated}
#' \item{Flow}{If TRUE/Yes}
#' \item{Level}{If TRUE/Yes}
#' \item{Sed}{If TRUE/Yes}
#' \item{OperSched}{Continuous or Seasonal}
#' \item{RealTime}{If TRUE/Yes}
#' \item{RHBN}{If TRUE/Yes the station is in the reference hydrologic basin network}
#' \item{Region}{ECCC Region}
#' \item{Datum}{Reference datum}
#' \item{Operator}{Operator}
#' }
#' 
"HYDAT_list"
NULL