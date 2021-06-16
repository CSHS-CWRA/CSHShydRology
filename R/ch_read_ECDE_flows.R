#' Reads a file of WSC daily flows from ECCCDataExplorer

#' @description Reads in a WSC daily flows file as returned from the program ECCCDataExplorer.
#'

#' @param filename Datafile retrieved from ECDataExplorer
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#'
#' @return Returns a dataframe with the last three rows removed and the Date as Date
#' @importFrom utils read.csv
#' @export

#' @examples \dontrun{
#' # Using a dummy file name as an example
#' mfile <- "04JD005_Daily_Flow_ts.csv"
#' mdata <- ch_read_ECDE_flows(mfile)}
#' 


ch_read_ECDE_flows <- function(filename) {
  mdata <- read.csv(filename)
  mdata$Date <- as.Date(mdata$Date, format = "%Y/%m/%d")
  cut <- length(mdata[, 1]) - 3
  mdata <- mdata[1:cut, ]
  # change "Flow,m3.c"   to "Flow" to deal with the difference in ECDE versions
  names(mdata)[4] <- substr(names(mdata)[4],1,4)

  if (names(mdata)[4] != "Flow") 
    print("File format error")
  
  return(mdata)
}
