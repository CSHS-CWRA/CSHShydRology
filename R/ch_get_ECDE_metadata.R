#' Reads Environment Canada Date Explorer (ECDE) meta data file
#'
#' @description Reads the file that is generated from ECDE 'save favourite stations' to capture the ECDE metadata. 
#' The dataframe returned contains 20 fields from ECDE.
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

#' @return Returns a dataframe consisting of:
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
#' \item{Reg.}{Regulated?} 
#' \item{Flow}{If TRUE/Yes flow data exists}
#' \item{Level}{If TRUE/Yes level data exists}
#' \item{Sed}{If TRUE/Yes sediment data exists}
#' \item{OperSched}{Operations current - Continuous or Seasonal}
#' \item{RealTime}{If TRUE/Yes real time data is available}
#' \item{RHBN}{If TRUE/Yes the stations is in the reference hydrologic basin network}
#' \item{Region}{Name of regional office operating station}
#' \item{Datum}{Elevation datum}
#' \item{Operator}{Operator or provider of the data}

#' @examples \dontrun{
#' # Don't run this example as it requires an ECDE file
#' filename <- "FavHydatStations.tb0"        # dummy file name (not supplied)
#' meta0 <- ch_get_ECDE_metadata(filename)
#' meta1 <- ch_get_ECDE_metadata(filename, writefile="study52_metadata.csv")
#' }
ch_get_ECDE_metadata  <- function(filename, writefile=NULL){
  
  # check ECDE filename
  if (filename == "" | is.null(filename)) {
    stop("ECDE file not specified")
  }
  
  if (!file.exists(filename)) {
    stop("ECDE file not found")
  }
  
  
  meta <- read.table(filename, skip = 96, sep = " ", na.strings = -999)
  
  names(meta) <- c("Station", "Fav", "StationName", "HydStatus", "Prov", "Latitude", 
                    "Longitude", "DrainageArea", "Years", "From", "To", "Reg.", 
                    "Flow", "Level", "Sed", "OperSched", "RealTime", "RHBN", 
                    "Region", "Datum", "Operator") 
  meta <- meta[,c(1, 3:21)]

    if (!is.null(writefile))  
    write.csv(meta, writefile, row.names = FALSE)
  return(meta)
}
