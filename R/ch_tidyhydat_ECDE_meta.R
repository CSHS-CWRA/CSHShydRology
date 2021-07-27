#' Creates an ECCCDE-like dataframe of metadata
#'
#' @description Extracts tombstone (meta) data for stations from \pkg{tidyhydat} in a 
#' format similar to that used by the Environment Canada Data Explorer (ECCCDE).
#'
#' @param stations  A vector of WSC station IDs, i.e. c("05BB001", "05BB003", "05BB004", "05BB005")
#' 
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' 
#' @export
#'
#' @return Returns a list with three items:
#' \itemize{
#' \item {\code{meta} - a dataframe of metadata from \pkg{tidyhydat} in ECDE form (not all ECDE fields are reproduced in this summary)}
#' \item {\code{H_version} - version information, and }
#' \item {\code{th_meta} - a dataframe with all \pkg{tidyhdat} fields including:}
#' \itemize{
#'   \item {Station - StationID}
#'   \item {StationName - Station Name}
#'   \item {HYDStatus - Active or Discontinued}
#'   \item {Prov - Province}
#'   \item {Latitude}
#'   \item {Longitude}
#'   \item {DrainageArea - km\eqn{^2}{^2}}
#'   \item {Years - number of years with data}
#'   \item {From - Start Year}
#'   \item {To - End Year}
#'   \item {Reg. - Regulated?} 
#'   \item {Flow - not captured (differs from ECDE)}
#'   \item {Level - not captured (differs from ECDE)}
#'   \item {Sed - not captured (differs from ECDE)}
#'   \item {OperSched - not captured (differs from ECDE)}
#'   \item {RealTime - if TRUE/Yes}
#'   \item {RHBN - if TRUE/Yes is in the reference hydrologic basin network}
#'   \item {Region - number of region instead of name (differs from ECDE)}
#'   \item {Datum - reference number (differs from ECDE)}
#'   \item {Operator - reference number (differs from ECDE)}
#' }
#' }
#' 
#' @importFrom tidyhydat hy_version hy_stations hy_stn_regulation hy_stn_data_range
#' @seealso \code{\link{ch_get_ECDE_metadata}} \code{\link{ch_tidyhydat_ECDE}}
#' @examples \dontrun{
#' stations <- c("05BB001", "05BB003", "05BB004", "05BB005")
#' result <- ch_tidyhydat_ECDE_meta(stations)
#' metadata <- result[[1]]
#' version <- result[[2]]}
#' 

ch_tidyhydat_ECDE_meta <- function(stations){

  H_version <- hy_version()  
  H_version <- data.frame(H_version)
  print(H_version)
  
  #extract difference parts of metadata using tidyhydat
  tc <- hy_stations(station_number=stations)
  tc <- data.frame(tc)
  
  td <- hy_stn_regulation(station_number=stations)
  td <- data.frame(td)
  
  te <- hy_stn_data_range(station_number=stations)
  te <- data.frame(te)
  te <- te[te[,2]=="Q",]
  
  colnmc <- c("Station",	"StationName","Prov",	"Region",	"HydStatus", "SedStatus",	"Latitude","Longitude",	"DrainageAreaG", "DrainageAreaE","RHBN",
             "RealTime",		"Contributor",	"Operator",	"Datum")
  colnmd <-c("Station",	"From",	"To",	"Reg.")
  colnme <-c("Station","DATA_TYPE","SED_DATA_TYPE","From",	"To",	"Years")
  colmeta <- c("Station",	"StationName","HydStatus","Prov",	 "Latitude","Longitude",	"DrainageArea", "Years","From","To", "Reg.",
               "Flow","Level","Sed","Opersched","RealTime","RHBN","Region","Datum","operator")
  
  names(tc) <-colnmc
  names(td) <-colnmd
  names(te) <-colnme
    
  t1 <- merge(tc,td, by.x="Station", by.y="Station")
  t2 <- merge(t1,te, by.x="Station", by.y="Station")
  t3 <- rep.int(NA, length(t2[,1]))
  th_meta <-t2
  
  meta <- data.frame(t2[,c(1:2,5,3,7:9,23,21:22,18)],t3,t3,t3,t3,t2[,c(12,12,4,15,14)])
  names(meta) <- colmeta
  
  result <- list(meta = meta, H_version = H_version, th_meta = th_meta )
  
  print("Result is a list that contains [1] metadata from tidyhydat in ECDE form, [2] H_version information, and [3] th_meta  ")
  print("not all ECDE fields are reproduced in this summary")
  
  return(result)
}
