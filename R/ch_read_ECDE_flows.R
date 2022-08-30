#' Reads a file of WSC daily flows from ECDataExplorer (ECDE)
#'
#' @description Reads in a file WSC daily flows as returned from the Windows program ECDataExplorer, converts the Date,
#' and omits the last 3 lines as these contain the data disclaimer and not data. The function
#' can read values from a url.
#'
#' @param filename Datafile retrieved from ECDataExplorer.
#'
#' @author Paul Whitfield 
#'
#' @return Returns a dataframe with the last three rows removed:
#' \item{ID}{stationID}
#' \item{PARAM}{Parameter 1 for Flow 2 for Level}
#' \item{Date}{original charater string converted to date format}
#' \item{Flow}{Daily mean flow m\eqn{^3}{^3}/sec}
#' \item{SYM}{Quality flag}
#' 
#' @importFrom utils read.csv
#' @export
#'
#' @examples \dontrun{
#' # Not run as requires a file returned by the Windows program ECDataExplorer
#' # Using a dummy file name as an example
#' mfile <- "04JD005_Daily_Flow_ts.csv"
#' mdata <- ch_read_ECDE_flows(mfile)}
#' 
#' \donttest{
#' # Not tested automatically as it is slow to read from a url
#' url1 <- "https://zenodo.org/record/7007830/files/08NL007_Daily_Flow_ts.csv"
#' values <- ch_read_ECDE_flows(url1)
#' }
#' 
ch_read_ECDE_flows <- function(filename) {
  
  # check ECDE filename
  if (filename == "" | is.null(filename)) {
    stop("ECDE file not specified")
  }
  
  if (!file.exists(filename)) {
    # check if actually a url
    result <- ch_test_url_file(filename, quiet = TRUE)
    if (result != "OK")
      stop("ECDE file not found")
  }
  mdata <- read.csv(filename, header = FALSE, skip = 1)
  names(mdata) <- c("ID", "PARAM", "Date", "Flow", "SYM")
  mdata$Date <- as.Date(mdata$Date, format = "%Y/%m/%d")
  cut <- length(mdata[, 1]) - 3
  mdata <- mdata[1:cut, ]

  if (names(mdata)[4] != "Flow") 
    message("Input data file format error")
  
  return(mdata)
}
