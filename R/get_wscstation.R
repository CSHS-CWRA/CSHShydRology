#' Retrieves station information for Water Survey of Canada sites
#'
#'
#' @param stnID - a Water Survey of Canada station number
#' @param stn default is HYDAT_list - station information from ECDataExplorer

#' @author Paul Whitfield <paul.h.whitfield@gmail.com>


#' @return a data frame with 21 elements
#' 
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
#' @export 
#' 
#'  
#' @examples
#' \dontrun{s_info <- get_wscstation("05BB001")}
 

get_wscstation <- function (stnID) {
  
  data("HYDAT_list", envir = environment())
  stn <- HYDAT_list
  rhbn <- NULL
  stninfo <- stn[stn$Station==stnID,]
  
  if(length(stninfo[,1])==0) {
   print(paste("WSC Station ",stnID," not found"))
   return(stnID) 
  } 

    if(stninfo$RHBN == TRUE) {
      rhbn <- "*" 
    }

  stninfo[21] <- paste(stninfo[1]," - ",stninfo[2], " - ", stninfo[4], rhbn, sep = "")
  names (stninfo) [21] <- "Station_lname"
  return (stninfo)
}

  
  

