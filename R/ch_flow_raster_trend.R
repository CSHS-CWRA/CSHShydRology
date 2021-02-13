#' Raster plot and simple trends of observed streamflows
#'
#' Creates a raster plot plus trend plots for day of year, and over time which may be binned by a number of days
#'
#' The plot contains four panels based upon binned data:
#' \enumerate{
#' \item The annual maximum, minimum, and median dlow with a trend test number
#' for each period: red arrows indicate decreases, blue arrows indicate increases.
#' \item The scale bar for the colours used in the raster plot,
#' \item The rasterplot with a colour for each
#' period and each year where data exist, and
#' \item A time series plot of the minimum, median, and maximum annual bin values. If there is no trend (p > 0.05) the points are black. Decreasing trend are in red, increasing trends are in blue..
#' }
#' 
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#'
#' @param date A numeric vector of the date as an \R date. Must be same length as the \code{flow}
#' @param flow A numeric vector of daily streamflows
#' @param step An integer indicating the degree of smoothing eg. 1, 5, 11.
#' @param stationID Station ID number, e.g. "05BB001". This value is optional, but is included in the output to help you identify the results.
#' @param title Title of the plot
#' @param missing If \code{FALSE} years with missing data are excluded. If \code{TRUE} partial years
#' are included.
#' @param colours A vector of colours used for the raster plot. The default is \code{c("lightblue","cyan", "blue", "slateblue", "darkblue", "red")}.
#
#' @return a list containing:
#' \describe{
#' 	\item{stationID}{Station ID eg 05BB001}
#' 	\item{missing}{How missing values were used FALSE=used, TRUE=removed}
#' 	\item{step}{number of days in a bin}
#' 	\item{periods}{number of periods in a year}
#' 	\item{period}{period numbers i.e. 1:365/step}
#' 	\item{bins}{values for each period in each year}
#' 	\item{med_period}{median for each period}
#' 	\item{max_period}{maximum for each period}
#' 	\item{min_period}{minimum for each period}
#' 	\item{tau_period}{Kendall's Tau for each period}
#' 	\item{prob_period}{probability of Tau for each period}
#' 	\item{year}{years spanning the data}
#' 	\item{median_year}{median bin for each year}
#' 	\item{max_year}{maximum bin for each year}
#' 	\item{min_year}{minimum bin for each year}
#' 	\item{tau_median_year}{value of tau and probability for median_year}
#' 	\item{tau_maximum_year}{value of tau and probability for max_year}
#' 	\item{tau_minimum_year}{value of tau and probability for min_year}
#' 	}
#' @keywords plot
#' @import stats graphics grDevices
#' @importFrom Kendall MannKendall
#' @importFrom fields image.plot
#' @importFrom graphics axis legend par plot points polygon
#' @export 
#' @seealso \code{\link{ch_flow_raster}}
#' @examples
#'  mdoy <- doys(W05AA008$Date)
#'  mplot <- ch_flow_raster_trend(W05AA008$Date, W05AA008$Flow, step = 5, station = "05AA008")
#'

ch_flow_raster_trend <- function(date, flow, step = 5, stationID = "", title = "", missing = FALSE,
                         colours = c("lightblue", "cyan", "blue", "slateblue", "darkblue", "red")) {
  l_disch <- expression(paste("m"^{3}, "/sec"))
  
  # get doy and year
  doy_vals <- doys(date)
  Year <- doy_vals$year
  doy <- doy_vals$doy
  DOY <- paste("Period of Year (", step, " day)", sep = "")

  if (step >= 31) {
    print("step has been reset to the maximum allowed [30] ")
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

  mslice <- slice(doy, step) ###  create a factor for n day periods
  myear <- as.factor(Year)
  fac <- list(myear, mslice)

  q_sliced <- tapply(flow, fac, median) # get median value for each bin.


  qsliced <- array(dim = c(periods, nYears))

  for (k in 1:length(aYears)) {
    for (kk in 1:periods) {
      qsliced[kk, (aYears[k] - nYear)] <- q_sliced[k, kk]
    }
  }


  colnames(qsliced) <- Years
  rownames(qsliced) <- period

  qmin <- min(flow, na.rm = TRUE)
  qmax <- max(flow, na.rm = TRUE)

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
    ymax_n[i] <- max(qsliced[, i], na.rm = missing)
    ymin_n[i] <- min(qsliced[, i], na.rm = missing)
  }
  ############################# replace -Inf with NA
  ymax_n[is.infinite(ymax_n)] <- NA
  ymin_n[is.infinite(ymin_n)] <- NA

  tcol <- c("red", "black", "blue")
  tmy <- MannKendall(ymed_n)
  tminy <- MannKendall(ymin_n)
  tmaxy <- MannKendall(ymax_n)

  t1 <- ifelse(as.numeric(tmy[2]) > 0.05, 2, ifelse(tmy[1] >= 0, 3, 1))
  t2 <- ifelse(as.numeric(tminy[2]) > 0.05, 2, ifelse(tminy[1] >= 0, 3, 1))
  t3 <- ifelse(as.numeric(tmaxy[2]) > 0.05, 2, ifelse(tmaxy[1] >= 0, 3, 1))


  #####################################################  three panel output

  par(oma = c(0, 0, 3, 0))
  qcols <- colorRampPalette(colours)

  # nf <- layout(matrix(c(2, 4, 1, 3), 2, 2, byrow = TRUE), c(3, 1), c(1, 3), TRUE)


  #####################################################  panel one raster image
  par(mar=c(6,4,0,0)) 
  
  image(1:periods,1:length(Years), qsliced, axes=FALSE, col=qcols(9),
                  zlim=c(qmin,qmax),  xlab="", ylab="")  
  
  sstep <-round(periods/5)
  speriod <- sub_set_Years(period,sstep)
  graphics::axis(1, at=speriod$position,labels=speriod$label, cex=1.2)
  
  nn<-1
  if(length(Years)>=70) nn <- 10 
  if(length(Years)>=40) nn <-  5
  if(length(Years)>=20) nn <-  2     
  sYears <- sub_set_Years(Years,nn)
  
  axis(2, at=sYears$position,labels=sYears$label, cex.axis=.7, las=1)
  mtext(DOY,side=1, line =2.2, cex=0.9)  
  box()
  
  month <- c("J","F","M","A", "M", "J", "J","A", "S", "O", "N", "D","") 
  mday <- c(0, 31, 59, 90, 120, 151, 181, 212,243, 273, 304, 334,365)
  md <- (mday / 365 * 73) + 1 
  
  axis(1,line=3.5,at=md, month)
  
  #####################################################  panel two doy summary of trends
  par(mar=c(7,4,0,0))
  mch   <- c("",1,19)
  mch_n <- c("",173,175)
  mcolour <- c("white","blue","red")
  
  ylimits<- c(min(qsliced,na.rm=TRUE),max(qsliced, na.rm=TRUE))
  par(mar=c(1,4,0,0))
  plot(period,med_n, ylab=l_disch, col="black", ylim=ylimits, xaxt="n", xaxs="i", las=1,pch=as.numeric(mch[code]))
  points(period,max_n, type="l", col="gray35")
  points(period,min_n, type="l", col="gray35")
  par(font=5)
  points(period,med_n, type="p", col=mcolour[arrow], pch=as.numeric(mch_n[arrow]), cex=1.2)
  par(font=1)
  
  axis(1,line=0,at=md, labels=FALSE)
  #####################################################  panel three time series
  options(scipen = 999)

  xy <- c(1:length(Years))
  ylimits<- c(min(qsliced,na.rm=TRUE),max(qsliced, na.rm=TRUE))
  if (ylimits[1] == 0) (ylimits[1]<-0.001)
  par(mar=c(6,4,0,0))
  plot(ymed_n,xy,  col = tcol[t1], xlim = ylimits, xlab = l_disch, yaxt = "n", yaxt = "n",yaxs = "i", log = "x", ylab="")
  points(ymax_n, xy, col=tcol[t3], pch=19, cex=0.7)
  points(ymin_n, xy, col=tcol[t2], pch=19, cex=0.7)
  
  
  ########################################################  Add title
  tscale = 1.2
  if(nchar(title) >= 45) tscale = 1.0
  if(nchar(title) >= 50) tscale = 0.8
  graphics::mtext(title, side=3, line=1, cex=tscale,outer=TRUE)
  
  
  ########################################################  Add scalebar
  graphics::frame()
  
  graphics::par(mar=c(0,0,0,4))
  
  zr=c(qmin,qmax)
  
  fields::image.plot( zlim= zr, col=qcols(9),legend.only=TRUE,
                      legend.width=4.5, legend.shrink=0.8,
                      bigplot=c(0.1,0.2,0.1,0.2),
                      legend.args=list(text=l_disch, side=2,line=0.5, cex=0.9))
  
  sID <- substr(title,1,7)
  
  
  line1 <-list(sID, missing, step, periods,qsliced,period,med_n,max_n,min_n,tau,prob, Years, 
               ymed_n,ymax_n,ymin_n, tmy, tmaxy,tminy)
  names(line1) <-c("sID","na.rm=", "step", "periods", "bins","period", "med_period","max_period","min_period", 	"tau_period", "prob_period",
                   "year", "median_year", "max_year", "min_year","tau_median_year", "tau_maximum_year", "tau_minimum_year")
  return(line1)
}

