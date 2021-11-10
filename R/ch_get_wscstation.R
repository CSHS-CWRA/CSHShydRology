#' Reads station information from a data file produced by ECDE
#'
#' @description Retrieves station information for an individual Water Survey of Canada site, based on stationID; 
#' adds a text string at position 21 that combines key elements for a title.
#'
#' @param stnID A Water Survey of Canada station number
#' @param metadata a data frame of station information from ECDataExplorer. The data frame \option{HYDAT_list} is supplied with this package.
#'
#' @author Paul Whitfield 
#'
#' @return Returns a line from a data frame with 21 variables
#' \item{Station}{StationID}
#' \item{StationName}{Station Name}
#' \item{HYDStatus}{Active or Discontinued}
#' \item{Prov}{Province}
#' \item{Latitude}{}
#' \item{Longitude}{}
#' \item{DrainageArea}{Area in km\eqn{^2}{^2}}
#' \item{Years}{# of years with data}
#' \item{From}{Start Year}
#' \item{To}{End Year}
#' \item{Reg.}{Regulated or natural}
#' \item{Flow}{if TRUE/Yes flow data is available}
#' \item{Level}{if TRUE/Yes water level data is available}
#' \item{Sed}{if TRUE/Yes sediment data is available}
#' \item{OperSched}{Current operation schedule- Continuous or Seasonal}
#' \item{RealTime}{if TRUE/Yes real itme data exists}
#' \item{RHBN}{if TRUE/Yes is in the reference hydrologic basin network}
#' \item{Region}{WSC Region}
#' \item{Datum}{Datum used}
#' \item{Operator}{Agency responsible for collecting data}
#' \item{Station_lname}{Added field combining StationID, StationName, Province and if station is RHBN an * is added}
#' 
#'
#' @export
#' 
#' @importFrom utils data
#'
#' @examples
#' data("HYDAT_list")
#' s_info <- ch_get_wscstation("05BB001", metadata = HYDAT_list)
#' title <- s_info[21]
#' print(title)
#'

ch_get_wscstation <- function(stnID, metadata = NULL) {

     HYDAT_list <- c(0)
  if (is.null(metadata)) {
    data("HYDAT_list", envir = environment())
    metadata <- HYDAT_list
  }

  
  rhbn <- NULL
  stninfo <- metadata[metadata$Station == stnID, ]

  if (length(stninfo[, 1]) == 0) {
    message(paste("WSC Station ", stnID, " not found"))
    return(stnID)
  }

  
  if (!is.na(stninfo$RHBN) && stninfo$RHBN == TRUE) {
    (rhbn <- "*")
  }

  stninfo[21] <- paste(stninfo$Station, " - ", stninfo$StationName, " - ", 
                       stninfo$Prov, rhbn, sep = "")
  names(stninfo) [21] <- "Station_lname"
  return(stninfo)
}
