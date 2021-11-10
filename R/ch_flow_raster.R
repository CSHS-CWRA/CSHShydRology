#' Raster plot of daily streamflows
#'
#' @description Produces a raster plot: years by day of year, showing magnitude of flow. 
#' This produces a plot showing the flow data in colours, showing different context than in
#' a hydrograph. High flows are in warm colours.
#' 
#' @param DF A data frame of daily flow data as read by \code{ch_read_ECDE_flows}.
#' @param rastercolours A vector of colours used for flow magnitudes (default \code{c("lightblue","cyan", "blue", "slateblue", "orange", "red")}).
#' @param metadata A dataframe of station metadata, defaults to \code{HYDAT_list}.
#' 
#' @return No value is returned; a standard R graphic is created.
#' @author Paul Whitfield 
#' @seealso \code{\link{ch_read_ECDE_flows}}
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics layout box
#' @importFrom timeDate dayOfYear as.timeDate
#' @importFrom fields image.plot
#' @seealso \code{\link{ch_flow_raster_trend}} \code{\link{ch_flow_raster_qa}}
#'
#' @examples
#' ch_flow_raster(CAN05AA008)
#' 
ch_flow_raster <- function(DF, rastercolours = c("lightblue","cyan", "blue", "slateblue", "orange", "red"),
                           metadata = NULL) {
  
  
  ##### Fixed labels and text strings
  DOY <- "Day of Year"
  ylabelq <- expression( paste("Discharge m"^{3}, "/sec"))
  
  qcols <- colorRampPalette(rastercolours)
  
  station <- as.character(DF$ID[1])
  sname <- ch_get_wscstation(station, metadata = metadata)
  title <- sname$Station_lname
  
  date <- as.Date(DF$Date, "%Y/%m/%d")
  
  Year <- as.numeric(format(date,"%Y"))
  doy <- as.numeric(dayOfYear(as.timeDate(date)))
  
  mYear <- max(Year, na.rm = TRUE)
  nYear <- min(Year, na.rm = TRUE) - 1
  Years <- mYear - nYear
  
  qdata <- array(dim = c(366, Years))
 
  rows <- doy[1:nrow(DF)]
  cols <- Year - nYear
  locs <- cbind(rows, cols)
  qdata[locs] <- DF$Flow
  qmax <- max(qdata, na.rm = TRUE)
  qmin <- min(qdata, na.rm = TRUE)
  
  ################################   raster map of daily flows
  qdata <- as.matrix(qdata)
  
  ########################################################### start  plotting section
  # capture plotting parameters, restore on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(oma = c(0, 0, 3, 0))
  layout(matrix(c(1, 1, 1, 1, 2, 1, 1, 1, 1, 3), 2, 5, byrow = TRUE))
  par(mar = c(4, 4, 0, 0))
  
  #################################################################  panel one
  
  doys <- c(1:366)
  lyears <- c((nYear + 1):mYear)
  
  image(1:366, 1:length(lyears), qdata,
        axes = FALSE, col = qcols(9),
        zlim = c(qmin, qmax), xlab = "", ylab = ""
  )
  
  sdoy <- ch_sub_set_Years(doys, 10)
  axis(1, at = sdoy$position, labels = sdoy$label, cex = 1.2)
  
  if (length(lyears) >= 70) nn <- 10 else nn <- 5
  sYears <- ch_sub_set_Years(lyears, nn)
  
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
  tscale <- 1.1
  if (nchar(title) >= 45) tscale <- 0.9
  if (nchar(title) >= 50) tscale <- 0.7
  mtext(title, side = 3, line = 1, cex = tscale, outer = TRUE)
  
  ############################################################### end plotting section
  
}
