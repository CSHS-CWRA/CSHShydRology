#'  Converts a tidyhydat daily flow data tibble to ECDE format
#'

#' @description Accessing daily flow data using \pkg{tidyhydat} is quick and efficient. However, it 
#' sometimes conflicts with other functions as \pkg{tidyhydat} changes variable names and some default 
#' entries. This function converts a tibble obtained from a \pkg{tidyhydat} tibble to a dataframe 
#' with standard Environment and Climate Change Canada Data Explorer (ECDE) names.
#'
#'
#' @param data Tibble of daily flows retrieved using \pkg{tidyhydat} function \code{hy_daily_flows}.

#' @author Paul Whitfield 
#'
#' @return A dataframe or a list of flows with formats consistent with datafiles read using \code{ch_read_ECDE_flows}:
#' \item{ID}{stationID}
#' \item{PARAM}{Parameter 1 for Flow 2 for Level}
#' \item{Date}{original charater string converted to date format}
#' \item{Flow}{Daily mean flow m\eqn{^3}{^3}/sec}
#' \item{SYM}{Quality flag}
#'  
#' @export
#' @seealso \code{\link{ch_tidyhydat_ECDE_meta}}
#' @examples \donttest{
#'library(tidyhydat)
#'mdata <- hy_daily_flow(station_number=c("05CK004"))
#'m_data <- ch_tidyhydat_ECDE(mdata)
#'
#'mdata <- hy_daily_flows(station_number=c("05CK004","08MF005","05BB001"))
#'mnew <- ch_tidyhydat_ECDE(mdata)
#'str(mnew[[1]])
#'str(mnew[[2]])
#'str(mnew[[3]]) 
#'#note the order is in increasing alphabetical order}

ch_tidyhydat_ECDE <- function(data) {
  
  ndata <- data.frame(data)   # untibble
 
  ndata$Parameter[ndata$Parameter == "Flow"] <- 1  #revert parameter to internal codes
  ndata$Parameter[ndata$Parameter == "Level"] <- 2
  ndata$Parameter <- as.integer(ndata$Parameter)
 
  ndata$Symbol[is.na(ndata$Symbol)] <- ""  #remove the NA that replaced "" in original
  
  result <- data.frame(ndata[,1], ndata[,3], ndata[,2], ndata[,4], ndata[,5])
  names(result) <- c("ID","PARAM","Date", "Flow", "SYM")  #assign the original names
  
  nstations <- unique(result$ID)

  if (length(nstations) == 1) {
    return(result)
  }
  
  if (length(nstations != 1)) {
  message(paste("Original tibble contained ",length(nstations),
              " stations. A list of dataframes is returned"))
   
  resulta <- split(result,result$ID)
  return(resulta)}
}