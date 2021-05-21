#' Raster plot of streamflows
#'
#' @description Produces a raster plot: years x day of year, showing magnitude of flows.  This produces a plot showing the flow data in colours, showing different context than in a hydrograph. High flows are in warm colours
#' @param dframe A data frame of hydrometric data. Must contain the variables \code{Date} and \code{Flow}.
#' @param title The (optional) title for the plot
#' @param rastercolours A vector of colours used for the raster plot. The default is \code{c("lightblue", "cyan", "blue", "slateblue", "orange", "red")}.
#' @return No value is returned; a standard \R graphic is created.
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @importFrom graphics axis legend par plot points polygon image frame mtext
#' @importFrom fields image.plot
#' @importFrom grDevices colorRampPalette
#' @export
#' @seealso \code{\link{ch_flow_raster_trend}} \code{\link{ch_flow_raster_qa}}
#'
#' @examples
#' ch_flow_raster(W05AA008)
#'
ch_flow_raster <- function(dframe, title="", rastercolours = c("lightblue", "cyan", "blue", "slateblue", "orange", "red")) {

  ##### Fixed labels and text strings
  DOY <- "Day of Year"
  ylabelq <- expression(paste("Discharge m"^{3}, "/sec"))
  
  date <- dframe$Date
  flow <- dframe$Flow
  qcols <- colorRampPalette(rastercolours)
  # get doy and year
  doy_vals <- doys(date)
  Year <- doy_vals$year
  doy <- doy_vals$doy
 
  mYear <- max(Year, na.rm = TRUE)
  nYear <- min(Year, na.rm = TRUE) - 1
  Years <- mYear - nYear

  qdata <- array(dim = c(366, Years))

  for (k in 1:length(date)) {
    qdata[doy[k], (Year[k] - nYear)] <- flow[k]
  }

  qmax <- max(qdata, na.rm = TRUE)
  qmin <- min(qdata, na.rm = TRUE)

  ################################   raster map of daily flows
  qdata <- as.matrix(qdata)

  ########################################################### start  plotting section
  par(oma = c(0, 0, 3, 0))
  layout(matrix(c(1, 1, 1, 1, 2, 1, 1, 1, 1, 3), 2, 5, byrow = TRUE))
  par(mar = c(4, 4, 1, 1))

  #################################################################  panel one

  doys <- c(1:366)
  lyears <- c((nYear + 1):mYear)

  image(1:366, 1:length(lyears), qdata,
    axes = FALSE, col = qcols(9),
    zlim = c(qmin, qmax), xlab = "", ylab = ""
  )

  sdoy <- sub_set_Years(doys, 10)
  axis(1, at = sdoy$position, labels = sdoy$label, cex = 1.2)

  if (length(lyears) >= 70) nn <- 10 else nn <- 5
  sYears <- sub_set_Years(lyears, nn)

  axis(2, at = sYears$position, labels = sYears$label, cex.axis = 1.2, las = 1)
  mtext(DOY, side = 1, line = 2.2, cex = 0.9)

  box()


  #################################################################  panel two
  frame()
  par(mar = c(2, 2, 2, 2))


  ######### scale bar and legend


  image.plot(
    zlim = c(qmin, qmax), col = qcols(9), legend.only = TRUE,
    legend.width = 4, legend.mar = 1,
    legend.shrink = 1.0,
    bigplot = c(0.1, 0.2, 0.1, 0.2),
    legend.args = list(text = ylabelq, side = 2, line = 0.5, cex = 0.90)
  )

  #################################################################  panel three (element #ten)

  frame()
  frame()
  frame()
  frame()
  frame()


  ##############################################################  Add title
  tscale <- 1.7
  if (nchar(title) >= 45) tscale <- 1.5
  if (nchar(title) >= 50) tscale <- 1.2
  mtext(title, side = 3, line = 0, cex = tscale, outer = TRUE)

}
