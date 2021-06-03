#' Reads Environment Canada Date Explorer (ECDE) meta data file
#'
#' @description Reads the file that is generated from ECDE 'save favourite stations' to capture the ECDE metadata. 
#' The dataframe returned contains 21 fields of metadata for each station.
#'  
#' @param filename The name of the ECDE file, \option{FavHydatStations.tb0}.
#' @param writefile Default is \code{NULL}, but if it is a filename e.g. \option{filename.csv} 
#' then the dataframe is saved to a csv file.
#' 
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#'
#' @importFrom utils read.table write.csv
#'
#' @export
#'
#' @return Returns a dataframe consisting of
#' \itemize{
#' \item	 {Station - StationID}
#' \item	 {StationName - Station Name}
#' \item	 {HYDStatus - Active or Discontinued}
#' \item	 {Prov - Province}
#' \item	 {Latitude}{}
#' \item	 {Longitude}{}
#' \item	 {DrainageArea - km2}
#' \item	 {Years - # of years with data}
#' \item	 {From - Start Year}
#' \item	 {To - End Year}
#' \item	 {Reg. - Regulated?} 
#' \item	 {Flow - if TRUE/Yes}
#' \item	 {Level - if TRUE/Yes}
#' \item	 {Sed - if TRUE/Yes}
#' \item	 {OperSched - Continuous or Seasonal}
#' \item	 {RealTime - if TRUE/Yes}
#' \item	 {RHBN - if TRUE/Yes is in the reference hydrologic basin network}
#' \item	 {Region - Name of regional office}
#' \item	 {Datum - Elevation datum}
#' \item	 {Operator - Operator}
#' }
#' @examples \dontrun{
#' filename <- "FavHydatStations.tb0"
#' meta0 <- ch_get_ECDE_metadata (filename)
#' meta1 <- ch_get_ECDE_metadata(filename, writefile="study52_metadata.csv")
#' }
ch_get_ECDE_metadata  <- function(filename, writefile=NULL){
  
  meta <- read.table(filename, skip = 96, sep = " ", na.strings =-999)
  
  names (meta) <-c("Station", "Fav", "StationName", "HydStatus", "Prov", "Latitude", "Longitude", "DrainageArea",
                   "Years", "From", "To", "Reg.", "Flow", "Level", "Sed", "OperSched", "RealTime", "RHBN", "Region", "Datum", "Operator") 
  meta <- meta[,c(1, 3:21)]

    if(!is.null(writefile))  
    write.csv(meta, writefile, row.names=FALSE)
  return(meta)
}
