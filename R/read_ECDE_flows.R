#' Reads a file of WSC daily flows from ECDataExplorer

#' Reads in a file WSC daily flows as returned from the program ECDataExplorer,
#' and omits the last 3 lines as these contain the data disclaimer.

#' @param filename Datafile retrieved from ECDataExplorer
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#'
#' @return Returns a dataframe with the last three rows removed and the Date as Date
#' @export

#' @examples 
#' mfile <- system.file("extdata", "04JD005_Daily_Flow_ts.csv", package = "CSHShydRology")
#' mdata <- read_ECDE_flows(mfile)
#' 


read_ECDE_flows <- function(filename) {
  mdata <- utils::read.csv(filename)
  mdata$Date <- as.Date(mdata$Date, format = "%Y/%m/%d")
  cut <- length(mdata[, 1]) - 3
  mdata <- mdata[1:cut, ]
  return(mdata)
}
