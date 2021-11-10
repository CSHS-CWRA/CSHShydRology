#' Raster plot and simple trends of observed streamflows by periods
#' 
#' @description
#' Creates a raster plot plus trend plots for day of year, 
#' which are binned by a number of days (step),
#' and the max, min, and median annual discharge across years. The plot contains four panels based upon binned data.
#'
#' @details
#' The four plots are: (1) The maximum,minimum,and median flow with a trend test for each 
#' period: red arrows indicate decreases, blue arrows indicate increases.
#' (2) The scale bar for the colours used in the raster plot,
#' (3) The raster plot with a colour for each period and each year where data exist, and
#' (4) A time series plot of the minimum, median, and maximum annual bin values.
#' If there is no trend (p > 0.05) the points are black. Decreasing trends are in red, increasing trends are in blue.
#' 
#' @author Paul Whitfield 
#' 
#' @param DF - dataframe of daily flow data as read by ch_read_ECDE_flows
#' @param step - a number indicating the degree of smoothing eg. 1, 5, 11.
#' @param missing If \code{FALSE} years with missing data are excluded. 
#' If \code{TRUE} partial years are included.
#' @param colours A vector of colours used for the raster plot. 
#' The default is \code{c("lightblue","cyan", "blue", "slateblue", 
#' "darkblue", "red")}.
#' @param metadata a dataframe of station metadata, default is HYDAT_list.
#'
#' @return Returns a list containing:
#'   \item{stationID}{Station ID eg. 05BB001}
#'   \item{missing}{How missing values were used FALSE = used, TRUE = removed}
#'   \item{step}{number of days in a bin}
#'   \item{periods}{number of periods in a year}
#'   \item{period}{period numbers i.e. 1:365/step}
#'   \item{bins}{values for each period in each year}
#'   \item{med_period}{median for each period}
#'   \item{max_period}{maximum for each period}
#'   \item{min_period}{minimum for each period}
#'   \item{tau_period}{Kendalls Tau for each period}
#'   \item{prob_period}{probability of Tau for each period}
#'   \item{year}{years spanning the data}
#'   \item{median_year}{median bin for each year}
#'   \item{max_year}{maximum bin for each year}
#'   \item{min_year}{minimum bin for each year}
#'   \item{tau_median_year}{value of tau and probability for annual median}
#'   \item{tau_maximum_year}{value of tau and probability for annual maximum}
#'   \item{tau_minimum_year}{value of tau and probability for annual minimum}
#'  
#'   
#' @keywords plot
#' @importFrom graphics axis legend par plot points polygon image frame mtext layout box
#' @importFrom grDevices colorRampPalette
#' @importFrom Kendall MannKendall
#' @importFrom fields image.plot
#' @importFrom stats median
#' @export 
#' @seealso \code{\link{ch_flow_raster}}
#' @references Whitfield, P. H., Kraaijenbrink, P. D. A., Shook, K. R., and Pomeroy, J. W. 2021.
#'  The Spatial Extent of Hydrological and Landscape Changes across the Mountains and Prairies 
#'  of Canada in the Mackenzie and Nelson River Basins Based on data from a Warm Season Time Window, 
#'  Hydrology and Earth Systems Sciences 25: 2513-2541.
#'
#'  
#' @examples
#'  data(CAN05AA008)
#'  mplot <- ch_flow_raster_trend(CAN05AA008, step=5)
#'
ch_flow_raster_trend <- function(DF, step = 5, missing = FALSE, metadata = NULL,
                                 colours = c("lightblue", "cyan", "blue", "slateblue", "darkblue", "red")) 
{
  l_disch <- expression(paste("m"^{3}, "/sec"))
  l_disch2 <- expression(paste("\nm" ^{3}, "/sec"))
  
  # get title information
  station <- DF[1, 1]
  sname <- ch_get_wscstation(station, metadata = metadata)
  title <- sname$Station_lname
  
  
  Date <- DF$Date
  Flow <- DF$Flow
  # get doy and year
  doy_vals <- ch_doys(Date)
  Year <- doy_vals$year
  doy <- doy_vals$doy
  DOY <- paste("Period of Year (", step, " day)", sep = "")
  
  if (step >= 31) {
    message(paste("step of", step, "larger than acceptable; has been reset to the maximum allowed [30] "))
    step <- 30
  }
  
  days <- 365
  periods <- days / step
  periods <- round(periods, digits = 0)
  period <- c(1:periods)
  ## Some records have stretches of missing years so the data needs to be reconfigured to individual years which have no record.
  
  mYear <- max(Year, na.rm = TRUE)
  nYear <- min(Year, na.rm = TRUE) - 1
  nYears <- mYear - nYear ## total number of years
  Years <- c((nYear + 1):mYear) ## all years in range
  aYears <- unique(Year) ## actual years in range
  
  mslice <- ch_slice(doy, step) ###  create a factor for n day periods
  myear <- as.factor(Year)
  fac <- list(myear, mslice)
  
  q_sliced <- tapply(Flow, fac, median)  # get median value for each bin.
  
  
  qsliced <- array(dim = c(periods, nYears))
  
  for (k in 1:length(aYears)) {
    for (kk in 1:periods) {
      qsliced[kk, (aYears[k] - nYear)] <- q_sliced[k, kk]
    }
  }


  colnames(qsliced) <- Years
  rownames(qsliced) <- period
  
  qmin <- min(Flow, na.rm = TRUE)
  qmax <- max(Flow, na.rm = TRUE)
  
  med_n <- array(NA, length(period))
  max_n <- array(NA, length(period))
  min_n <- array(NA, length(period))
  
  tau <- array(NA, length(period))
  prob <- array(NA, length(period))
  code <- array(2, length(period))
  arrow <- array(1, length(period))
  
  for (i in 1:length(period)) { ### loop over getting values for periods of year
    med_n[i] <- median(qsliced[i, ], na.rm = TRUE)
    max_n[i] <- max(qsliced[i, ], na.rm = TRUE)
    min_n[i] <- min(qsliced[i, ], na.rm = TRUE)
    
    max_n[is.infinite(max_n)] <- NA
    min_n[is.infinite(min_n)] <- NA
    
    
    t1 <- NA
    t1 <- MannKendall(qsliced[i, ])
    tau[i] <- t1$tau
    prob[i] <- t1$sl
    
    # set flags for plotting
    if (abs(prob[i]) == 1.00) code[i] <- 1
    if (prob[i] <= 0.05) code[i] <- 3
    
    if (prob[i] <= 0.05) arrow[i] <- 2
    if (prob[i] <= 0.05 && tau[i] <= 0.) arrow[i] <- 3
  }
  
  ymed_n <- array(NA, length(Years))
  ymax_n <- array(NA, length(Years))
  ymin_n <- array(NA, length(Years))
  
  for (i in 1:length(Years)) { 
    ### loop over getting values for each year
    ymed_n[i] <- median(qsliced[, i], na.rm = missing)
    ymax_n[i] <- max(qsliced[ , i], na.rm = missing)
    ymin_n[i] <- min(qsliced[ , i], na.rm = missing)
  }
  
  ############################# replace -Inf with NA
  ymax_n[is.infinite(ymax_n)] <- NA
  ymin_n[is.infinite(ymin_n)] <- NA
  
  tcol <- c("red", "black", "blue")
  
  tmy   <- MannKendall(ymed_n)
  tminy <- MannKendall(ymin_n)
  tmaxy <- MannKendall(ymax_n)
  
  t1 <- ifelse(as.numeric(tmy[2])  > 0.05, 2, ifelse(tmy[1]  >= 0, 3, 1))
  t2 <- ifelse(as.numeric(tminy[2]) > 0.05, 2, ifelse(tminy[1] >= 0, 3, 1))
  t3 <- ifelse(as.numeric(tmaxy[2]) > 0.05, 2, ifelse(tmaxy[1] >= 0, 3, 1))
  
  
  #####################################################  three panel output
  
  # capture plotting parameters, restore on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(oma = c(1, 1, 3, 1))
  qcols <- colorRampPalette(colours)
  
  nf <- layout(matrix(c(2, 4, 1, 3), 2, 2, byrow = TRUE), c(3, 1), c(1, 3), TRUE)
  
  
  #####################################################  panel one raster image
  par(mar = c(6, 4, 0, 0))  
  
  image(1:periods, 1:length(Years), qsliced, axes = FALSE, col = qcols(9),
        zlim = c(qmin, qmax),  xlab = "", ylab = "")  
  
  sstep <- round(periods/5)
  speriod <- ch_sub_set_Years(period,sstep)
  axis(1, at = speriod$position, labels = speriod$label, cex = 1.2)
  
  nn <- 1
  if (length(Years) >= 70) nn <- 10 
  if (length(Years) >= 40) nn <-  5
  if (length(Years) >= 20) nn <-  2
  sYears <- ch_sub_set_Years(Years, nn)
  
  axis(2, at = sYears$position, labels = sYears$label, cex.axis = .7, las = 1)
  mtext(DOY,side = 1, line = 2.2, cex = 0.9)  
  box()
  
  month <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "")  #***
  mday <- c(0, 31, 59, 90, 120, 151, 181, 212,243, 273, 304, 334,365)
  md <- (mday / 365*as.integer(365 / step)) + 1
  
  axis(1, line = 3.5, at = md, month)
  
  #####################################################  panel two doy summary of trends
  par(mar = c(7, 4, 0, 0))
  mch   <- c("", 1, 19)
  mch_n <- c("", 173, 175)
  mcolour <- c("white", "blue", "red")
  
  ylimits <- c(min(qsliced, na.rm = TRUE), max(qsliced, na.rm = TRUE))
  par(mar = c(1, 4, 0, 0))
  plot(period, med_n, ylab = l_disch, col = "black", ylim = ylimits, xaxt = "n", xaxs = "i", las = 1,pch = as.numeric(mch[code]))
  points(period, max_n, type = "l", col = "gray35")
  points(period, min_n, type = "l", col = "gray35")
  par(font = 5)
  points(period, med_n, type = "p", col = mcolour[arrow], pch = as.numeric(mch_n[arrow]), cex = 1.2)
  par(font = 1)
  
  axis(1, line = 0, at = md, labels = FALSE)
  #####################################################  panel three time series
  options(scipen = 999)
  
  xy <- c(1:length(Years))
  ylimits <- c(min(qsliced, na.rm = TRUE), max(qsliced, na.rm = TRUE))
  if (ylimits[1] == 0) (ylimits[1] <- 0.001)
  par(mar = c(6,4,0,0))
  plot(ymed_n, xy, col = tcol[t1], xlim = ylimits, xlab = l_disch, yaxt = "n", yaxt = "n",
       yaxs = "i", log = "x", ylab = "")
  points(ymax_n, xy, col = tcol[t3], pch = 19, cex = 0.7)
  points(ymin_n, xy, col = tcol[t2], pch = 19, cex = 0.7)
  
  
  ########################################################  Add title
  tscale <- 1.2
  if (nchar(title) >= 45) tscale <- 1.0
  if (nchar(title) >= 50) tscale <- 0.8
  mtext(title, side = 3, line = 1, cex = tscale,outer = TRUE)
  
  
  ########################################################  Add scale bar
  frame()
  
  par(oma = c(2, 9, 0, 0))
  par(mar = c(0, 0, 0, 0))
  
  
  zr = c(qmin, qmax)
  
  image.plot(zlim = zr, 
             col = qcols(9),legend.only = TRUE,
             legend.width = 4.5, legend.shrink = 0.8,
             bigplot = c(0.1, 0.2, 0.1, 0.2),
             legend.args = list(text = l_disch, side = 2, line = 0.5, cex = 0.9))
  
  sID <- substr(title, 1, 7)
  
  line1 <- list(sID, missing, step, periods, qsliced, period, med_n, max_n, min_n,
                tau, prob, Years, ymed_n, ymax_n, ymin_n, tmy, tmaxy, tminy)
  names(line1) <- c("sID", "na.rm =", "step", "periods", "bins","period", "med_period", "max_period",
                   "min_period", "tau_period", "prob_period", "year", "median_year", "max_year", "min_year", 
                   "tau_median_year", "tau_maximum_year", "tau_minimum_year")
  return(line1)
}
