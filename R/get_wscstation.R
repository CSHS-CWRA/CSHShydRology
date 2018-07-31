#' Reads WSC station information from a data file
#' Retrieves station information for an individual Water Survey of Canada site,
#' adds a text string at position 21 that combines key elements for a title.
#'
#' @param stnID A Water Survey of Canada station number
#' @param stn a data frame of station information from ECDataExplorer. The data frame \option{HYDAT_list} is supplied with this package.
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#'
#' @return Returns a data frame with 21 variables
#' \itemize{
#' \item	 {Station}  {StationID}
#' \item	 {StationName} {Station Name}
#' \item	 {HYDStatus} {Active or Discontinued}
#' \item	 {Prov} {Province}
#' \item	 {Latitude}{}
#' \item	 {Longitude}{}
#' \item	 {DrainageArea} {km2}
#' \item	 {Years} {# of years with data}
#' \item	 {From} {Start Year}
#' \item	 {To} {End Year}
#' \item	 {Reg.} {Regulated }
#' \item	 {Flow} {if TRUE/Yes}
#' \item	 {Level} {if TRUE/Yes}
#' \item	 {Sed} {if TRUE/Yes}
#' \item	 {OperSched} {Continuous or Seasonal}
#' \item	 {RealTime} {if TRUE/Yes}
#' \item	 {RHBN} {if TRUE/Yes is in the reference hydrologic basin network}
#' \item	 {Region} {if TRUE/Yes is in the reference hydrologic basin network}
#' \item	 {Datum} {if TRUE/Yes is in the reference hydrologic basin network}
#' \item	 {Operator} {if TRUE/Yes is in the reference hydrologic basin network}
#' \item   {Station_lname} {Added field combines ID,Name,Province and if RHBN an * is added}
#' }
#'
#' @export
#'
#' @examples
#' df <- HYDAT_list
#' s_info <- get_wscstation("05BB001", df)
#' title <- s_info[21]
#' print(title)
#'

get_wscstation <- function(stnID, stn) {

  rhbn <- NULL
  stninfo <- stn[stn$Station == stnID, ]

  if (length(stninfo[, 1]) == 0) {
    print(paste("WSC Station ", stnID, " not found"))
    return(stnID)
  }

  if (stninfo$RHBN == TRUE) {
    (rhbn <- "*")
  }

  stninfo[21] <- paste(stninfo$Station, " - ", stninfo$StationName, " - ", stninfo$Prov, rhbn, sep = "")
  names(stninfo) [21] <- "Station_lname"
  return(stninfo)
}
