#' Raster plot of daily streamflows with WSC quality flags
#'
#' @description Raster plot with WSC quality flags. 
#' This produces a plot showing the flow data in grayscale 
#' overlain by the Water Survey of Canada quality flags.  Colours are consistent with 
#' ECDataExplorer. Raster layout lets the use see the flags in a different context than in 
#' a hydrograph.
#' 
#' @return
#' Produces a raster plot: years against day of year, showing the data flags:
#'  \item{A}{(Partial)  in green}
#'  \item{B}{(Backwater) in cyan}
#'  \item{D}{(Dry) in yellow}
#'  \item{E}{(Estimated) in red}
#'
#' 
#' @param DF dataframe of daily streamflow read by ch_read_ECDE_flows
#' @seealso \code{\link{ch_read_ECDE_flows}}
#' @param metadata dataframe of metadata or defaults to "HYDAT_list"
#' 
#' @return Returns \code{TRUE} if executed properly; a standard R graphic is created.
#' 
#' @author Paul Whitfield 
#' @importFrom graphics axis legend par plot points polygon image frame mtext layout box
#' @importFrom grDevices colorRampPalette
#' @importFrom timeDate dayOfYear as.timeDate 
#' @importFrom fields image.plot 
#' @export 
#' @seealso \code{\link{ch_flow_raster}}
#'
#' @examples
#' data(HYDAT_list)
#' data(CAN05AA008)
#' qaplot <- ch_flow_raster_qa(CAN05AA008)
#' 
ch_flow_raster_qa <- function(DF, metadata = NULL) {
  ##### Fixed labels and text strings
  DOY <- "Day of Year"
  ylabelq <- expression(paste("Discharge m"^{3}, "/sec"))
  
  rastercolours <- c("gray90", "gray80", "gray70", "gray60", "gray50", "gray40", "gray30", "gray20", "gray10")
  qcols <- colorRampPalette(rastercolours)
  
  fcols <- c("black", "red", "green", "blue", "cyan", "magenta", "yellow", "gray")
  
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
  flag  <- array(dim = c(366, Years))
  
  DF$SYM <- as.character(DF$SYM)   #############    change flag codes to colour codes
  DF$SYM[DF$SYM == "A"] <- 3
  DF$SYM[DF$SYM == "B"] <- 5
  DF$SYM[DF$SYM == "D"] <- 7
  DF$SYM[DF$SYM == "E"] <- 2
  DF$SYM <- as.numeric(DF$SYM)   ##### convert to numbers for plotting colour
  
  
  for (k in 1:length(DF[ , 1])) {
    
    qdata[doy[k], (Year[k] - nYear)] <- DF$Flow[k]
    
    flag[doy[k], (Year[k] - nYear)] <- DF$SYM[k]
    # 1 was no flag, 2 was "A", 3 was "B", 4 was "C", 5 was "D", 6 was "E"
  }
  flag[flag == 1] <- NA
  
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
  
  sdoy <- ch_sub_set_Years(doys, 15)
  axis(1, at = sdoy$position, labels = sdoy$label, cex = 1.2)
  
  if (length(lyears) >= 70) nn <- 10 else nn <- 5
  sYears <- ch_sub_set_Years(lyears, nn)
  
  axis(2, at = sYears$position, labels = sYears$label, cex.axis = 1.2, las = 1)
  mtext(DOY, side = 1, line = 2.2, cex = 0.9)
  
  for (ii in 1:366) {
    for (jj in 1:length(lyears)) {
      points(ii, jj,pch = 15,col = flag[ii, jj])
    }
  }
  
  box()
  
  #################################################################  panel two
  
  frame()
  par(mar = c(4, 4, 0, 0))
  
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
  par(oma = c(0, 0, 2, 0))
  par(mar = c(1, 0, 0, 1))
  leg.txt <- c(" (A) Partial", " (B) Backwater", " (D) Dry", " (E) Estimate")
  lcol <- c("green", "cyan", "yellow", "red")
  legend("left", leg.txt, pch = c(22, 22, 22, 22), pt.bg = lcol, cex = 1.0, bty = "n")
  
  ##############################################################  Add title
  
  tscale <- 1.3
  if (nchar(title) >= 45) tscale <- 1.1
  if (nchar(title) >= 50) tscale <- 0.8
  mtext(title, side = 3, line = 1, cex = tscale, outer = TRUE)
  
  ############################################################### end plotting section
  return(TRUE)
}
