#' Compares two time periods of data using Mann-Whitney test
#' 
#' @description Compares two time periods of data using the Mann-Whitney test. 
#' Data are binned based upon a bin size, and data are extracted for two time periods 
#' and tests for change between two such periods
#' result can be passed to \code{ch_polar_plot} or \code{ch_decades_plot} 
#' for visualization.
#' 
#' @author Paul Whitfield 
#' @references
#' Whitfield, P.H., Cannon, A.J., 2000. Recent variations in climate and 
#' hydrology in Canada. Canadian Water Resources Journal 25: 19-65. 
#' 
#' @param DF A data frame of hydrometric data from \code{ch_read_ECDE_flows}
#' @param step An integer indicating the degree of smoothing eg. 1, 5, 11.
#' @param range1 The first and last year of first period, as \code{c(first,last)}
#' @param range2 The first and last year of second period, as \code{c(first,last)}
#' @param ptest The significance level default is \code{0.05}.
#' @param variable Name of variable. Default is \option{discharge}
#' @param metadata dataframe of station metadata, default is HYDAT_list
#' 
#' @return Returns a list containing:
#'  \item{StationID}{ID of station}
#'  \item{Station_lname}{Name of station}
#'  \item{bin_width}{Smoothing time step}
#'  \item{range1}{First range of years}
#'  \item{range2}{Second range of years}
#'  \item{p_used}{p_value}
#'  \item{fail}{TRUE if test failed due to missing values}
#'  \item{bin_method}{method used for binning}
#'  \item{test_method}{Mann-Whitney U-statistic}
#'  \item{series}{a data frame containing:}
#' 	\item{period}{period numbers i.e. 1:365/step}
#' 	\item{period1}{median values for each bin in period 1}
#' 	\item{period2}{median values for each bin in period 2}
#' 	\item{mwu}{Mann-Whitney U-statistic for each bin between the two periods}
#' 	\item{prob}{probability of U-statistic for each period}
#' 	\item{code}{significance codes for each bin}
#' 
#' @importFrom stats wilcox.test median
#' @export
#' @seealso \code{\link{ch_polar_plot}} \code{\link{ch_polar_plot_prep}} 
#' \code{\link{ch_decades_plot}}
#' 
#' @examples
#' data(HYDAT_list)
#' data(CAN05AA008)
#' # first example fails due to missing data in both periods
#' range1 <- c(1960,1969)
#' range2 <- c(1990,1999)
#' b_MW <- ch_binned_MannWhitney(CAN05AA008, step = 5, range1, range2, ptest = 0.05)
#'
#' range1 <- c(1970,1979)
#' range2 <- c(1990,1999)
#' b_MW <- ch_binned_MannWhitney(CAN05AA008, step = 5, range1, range2, ptest = 0.05)
#' 
ch_binned_MannWhitney <- function(DF, step, range1, range2, ptest=0.05, variable="discharge", 
                                  metadata = NULL) {
  
  fail <- FALSE
  mdoy <- ch_doys(DF$Date)
  doy <- mdoy$doy
  years <- mdoy$year
  flow <- DF$Flow
  
  sID <- as.character(DF[1,1])
  sname <- ch_get_wscstation(sID, metadata = metadata)
  
  binmethod <- "median"
  testmethod <- "Mann-Whitney U"
  
  days <- 365
  periods <- days / step
  periods <- round(periods, digits = 0)
  period <- c(1:periods)
  ## Some records have stretches of missing years so the data needs to be reconfigured to individual years which have no record.
  
  mYear <- max(years, na.rm = TRUE)
  nYear <- min(years, na.rm = TRUE) - 1
  nYears <- mYear - nYear ## total number of years
  Years <- c((nYear + 1):mYear) ## all years in range
  aYears <- unique(years) ## actual years in range
  
  
  mslice <- ch_slice(doy, step) ###  create a factor for n day periods
  myear <- as.factor(years)
  fac <- list(myear, mslice)
  qsliced <- array(dim = c(nYears, periods))
  
  q_sliced <- tapply(flow, fac, median) # get median value for each bin.
  
  # qliced contains median for periods and for only year where data existed. Need to reform so missing years are included
  
  
  locs <- 1:length(aYears)
  qsliced[aYears[locs] - nYear, ] <- q_sliced[locs, ]
  
  colnames(qsliced) <- period
  rownames(qsliced) <- Years
  
  # set up arrays for results
  period1 <- array(NA, length(period))
  period2 <- array(NA, length(period))
  mwu <- array(NA, length(period))
  prob <- array(NA, length(period))
  code <- array(0, length(period))
  
  rg1 <- c(range1 - nYear)
  rg2 <- c(range2 - nYear)
  
  for (i in 1:length(period)) { ### loop over getting values for periods of year
    
    s1 <- qsliced[rg1[1]:rg1[2], i]
    s2 <- qsliced[rg2[1]:rg2[2], i]
    sout <- wilcox.test(s1, s2, exact = FALSE)
    
    period1[i] <- median(s1)
    period2[i] <- median(s2)
    mwu[i] <- sout[[1]]
    prob[i] <- sout[[3]]
    if (prob[i] <= ptest) code[i] <- (mean(s1) - mean(s2)) / (abs(mean(s1) - mean(s2)))
  }
  
  if (length(period1[!is.na(period1)]) != length(period1)) {
    message("Range_1 contains missing values")
    fail <- TRUE
  }
  if (length(period2[!is.na(period2)]) != length(period2)) {
    message("Range_2 contains missing values")
    fail <- TRUE
  }
  
  series <- data.frame(period, period1, period2, mwu, prob, code)
  names(series) <- c("period", "median_1", "median_2", "MW_U", "p_value",
                     "s_code")
  
 
  
  result <- list(sID, sname[21], variable, step, range1, range2, ptest, fail, binmethod, testmethod, series)
  
  names(result) <- c(
    "StationID", "Station_lname", "variable", "bin_width", "range1", "range2",
    "p_used", "fail", "bin_method", "test_method", "series"
  )
  
  return(result)
}
