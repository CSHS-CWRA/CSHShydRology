#'  Plots a hydrograph with the data quality symbols and returns a report on qa symbols and missing data.
#'  
#' @description Plots a hydrograph of a WSC daily data file read from from ECDataExplorer (ECDE). 
#' The hydrograph shows individual days with data quality symbols [SYM] 
#' in colour and counts cases of each and reports them in the legend. The colours and symbols 
#' are those produced by ECDataExplorer. 
#' 
#' There is an option is to provide start and end dates to show 
#' only part of the time period for which data exists and the plot is annotated to indicate this. 
#' Counts of missing observations is also provided in the legend.
#'
#' @param DF Data frame retrieved from ECDataExplorer as returned by the function 
#' \code{ch_read_ECDE_flows}.
#' @param st_date Optional start date in the form \option{yyyy-mm-dd}. Default is \code{NULL}.
#' @param end_date Optional end date in the form \option{yyyy-mm-dd}. Default is \code{NULL}.
#' @param rescale If \code{FALSE} (the default), the y-axis scaling is determined by the time 
#' period. If \code{TRUE} then determined by the whole dataset.
#' @param sym_col Colours used for SYM; default is those used in ECDE ("black",
#'  "green", "cyan","yellow", "red", "white").  The final "white" can be changed to highlight
#'  missing data points.
#' @param cts If \code{TRUE} (the default) shows the counts of SYM in the legend. If \code{FALSE} 
#' the counts are omitted as in ECDE.
#' @param metadata a dataframe of station metadata, default is \code{HYDAT_list}.
#' 
#' 
#' @author Paul Whitfield 
#'
#' @return Produces a plot and returns a list that contains:
#' \item{station name or title used}{}
#' \item{st_date}{starting date}
#' \item{end_date}{ending data}
#' \item{n}{the number of data points}
#' \item{sym_count}{summary of the SYM counts}
#' \item{missing}{number of missing data} 
#' 
#' 
#' @export
#' 
#' @importFrom graphics text
#'
#' @examples
#' m_test <- ch_qa_hydrograph(CAN05AA008)
#  using a date range
#' m_test <- ch_qa_hydrograph(CAN05AA008, st_date="1980-01-01", end_date="1999-12-31")
#

ch_qa_hydrograph <- function(DF, st_date = NULL, end_date = NULL, cts = TRUE, rescale = FALSE, 
                             sym_col = c("black", "green", "cyan", "yellow", "red", "white"),
                             metadata = NULL) {
  
  
  disch <- expression(paste("Mean Daily Discharge m"^{3}, "/sec"))
  
  m_station <- ch_get_wscstation(DF[1, 1], metadata = metadata)
  title <- paste(m_station$Station, "  ", m_station$StationName)
  
  sym_count <- array(0, dim = 6)
  DF$dcol <- array(1, dim = length(DF[ , 1]))
  
  
  ylims <- c(min(DF[ , 4]),max(DF[,4]))
  
  if (!is.null(st_date))
  {
    st_date <- as.Date(st_date, "%Y-%m-%d")
    end_date <- as.Date(end_date, "%Y-%m-%d")
    
    DF <- DF[DF$Date >= st_date, ]
    DF <- DF[DF$Date <= end_date, ]
  }
  
  if (!rescale) ylims <- c(min(DF[ , 4]), max(DF[ , 4]))
  
  for (k in 1:length(DF$dcol))
  {
    if (!is.na(DF$SYM[k]) && DF$SYM[k] == "A") {DF$dcol[k] <- 2; sym_count[2] <- sym_count[2] + 1}
    if (!is.na(DF$SYM[k]) && DF$SYM[k] == "B") {DF$dcol[k] <- 3; sym_count[3] <- sym_count[3] + 1}
    if (!is.na(DF$SYM[k]) && DF$SYM[k] == "C") {DF$dcol[k] <- 4; sym_count[4] <- sym_count[4] + 1}
    if (!is.na(DF$SYM[k]) && DF$SYM[k] == "D") {DF$dcol[k] <- 5; sym_count[5] <- sym_count[5] + 1}
    if (!is.na(DF$SYM[k]) && DF$SYM[k] == "E") {DF$dcol[k] <- 6; sym_count[6] <- sym_count[6] + 1}
  }
  
  par(mar = c(2.5, 4.5, 3, 1))
  plot(DF$Date, DF[ , 4],
       col = "black", type = "l", ylim = ylims,  ylab = disch, xlab = "", main = title, lwd = 0.2, las = 1)
  points(DF$Date, DF[ , 4], col = sym_col[DF$dcol], type = "p", pch = 19, cex = 0.6)
  
  sym_count[1] <- length(DF[,4]) - sum(sym_count)
  missdays <- as.numeric(1 + DF[length(DF[,1]), 3] - DF[1,3] - length(DF[, 1]))
  
  ltexta <- c("Default  ","(A) - Partial  ","(B) - Backwater  ","(D) - Dry  ","(E) - Estimate  ")
  ltextb <- c(paste("Default  ", sym_count[1]), 
              paste("(A) - Partial  ", sym_count[2]),
              paste("(B) - Backwater  ", sym_count[3]), 
              paste("(D) - Dry  ", sym_count[5]), 
              paste("(E) - Estimate  ", sym_count[6]),
              paste("Missing", missdays))
  
  
  
  if (!cts)  legend("topleft", ltexta, pch = 19, col = sym_col, cex = 0.7, bg = "transparent")
  if (cts)   legend("topleft", ltextb, pch = 19, col = sym_col, cex = 0.7, bg = "transparent")
  
  if (!is.null(st_date))  text(DF$Date[as.integer(0.75 * length(DF$Date))], max(DF[ , 4]),
                               "Selected Date Range", col = "gray50", cex = 0.7)
  
  names(sym_count) <- c("Default", "A", "B", "C", "D", "E")
  
  result <- list(title,  st_date, end_date, length(DF[,4]), sym_count, missdays)
  names(result) <- c("Station", "start_date", "end_date", "points", "SYM_count","missing_observations")
  return(result)
}
