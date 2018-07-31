#' Raster plot of streamflows with WSC quality flags.
#'
#' Produces a raster plot of years x day of year showing the flow data in grayscale
#' overlain by the Water Survey of Canada quality flags. Colours are consistent with
#' ECDataExplorer. Raster layout lets the use see the flags in a different context than in
#' a hydrograph.
#' The data flags are:
#' \describe{
#' \item{A (Partial)}{green}
#' \item{B (Below Ice)}{blue}
#' \item{D (Dry)}{yellow}
#' \item{E (Estimated)}{red}
#' }
#' @param dframe A data frame of WSC hydrometric data. Must contain the variables \code{Date}, \code{Flow} and \code{SYM} which is the WSC data flag symbol.
#' @param title The (optional) title for the plot
#' @return No value is returned; a standard \R graphic is created.
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @export
#' @seealso \code{\link{flow_raster_trend}} \code{\link{flow_raster}}
#'

#' @examples
#' flow_raster_qa(W05AA008, "Station W05AA008")

flow_raster_qa <- function(dframe, title = "") {
  ##### Fixed labels and text strings
  DOY <- "Day of Year"
  ylabelq <- expression(paste("Discharge m"^{3}, "/sec"))

  rastercolours <- c("gray90", "gray80", "gray70", "gray60", "gray50", "gray40", "gray30", "gray20", "gray10")
  qcols <- grDevices::colorRampPalette(rastercolours)
  date <- as.Date(dframe$Date, "%Y/%m/%d")

  Year <- as.numeric(format(date, "%Y"))
  doy <- as.numeric(timeDate::dayOfYear(timeDate::as.timeDate(date)))

  mYear <- max(Year, na.rm = TRUE)
  nYear <- min(Year, na.rm = TRUE) - 1
  Years <- mYear - nYear

  qdata <- array(dim = c(366, Years))
  flag <- array(dim = c(366, Years))

  dframe$SYM <- as.character(dframe$SYM) #############    change flag codes to colour codes
  dframe$SYM[dframe$SYM == "A"] <- 3
  dframe$SYM[dframe$SYM == "B"] <- 5
  dframe$SYM[dframe$SYM == "D"] <- 7
  dframe$SYM[dframe$SYM == "E"] <- 2
  dframe$SYM <- as.numeric(dframe$SYM) ##### convert to numbers for plotting colour


  for (k in 1:length(dframe[, 1])) {
    qdata[doy[k], (Year[k] - nYear)] <- dframe$Flow[k]

    flag[doy[k], (Year[k] - nYear)] <- dframe$SYM[k]
    # 1 was no flag, 2 was "A", 3 was "B", 4 was "C", 5 was "D", 6 was "E"
  }
  flag[flag == 1] <- NA

  qmax <- max(qdata, na.rm = TRUE)
  qmin <- min(qdata, na.rm = TRUE)


  ################################   raster map of daily flows
  qdata <- as.matrix(qdata)

  ########################################################### start  plotting section
  graphics::par(oma = c(0, 0, 3, 0))
  graphics::layout(matrix(c(1, 1, 1, 1, 2, 1, 1, 1, 1, 3), 2, 5, byrow = TRUE))
  graphics::par(mar = c(4, 4, 1, 1))

  #################################################################  panel one

  doys <- c(1:366)
  lyears <- c((nYear + 1):mYear)

  graphics::image(1:366, 1:length(lyears), qdata,
    axes = FALSE, col = qcols(9),
    zlim = c(qmin, qmax), xlab = "", ylab = ""
  )

  sdoy <- sub_set_Years(doys, 15)
  graphics::axis(1, at = sdoy$position, labels = sdoy$label, cex = 1.2)

  if (length(lyears) >= 70) nn <- 10 else nn <- 5
  sYears <- sub_set_Years(lyears, nn)

  graphics::axis(2, at = sYears$position, labels = sYears$label, cex.axis = 1.2, las = 1)
  graphics::mtext(DOY, side = 1, line = 2.2, cex = 0.9)

  for (ii in 1:366) {
    for (jj in 1:length(lyears)) {
      graphics::points(ii, jj, pch = 15, col = flag[ii, jj])
    }
  }

  graphics::box()


  #################################################################  panel two
  graphics::frame()
  graphics::par(mar = c(2, 2, 2, 2))


  ######### scale bar and legend

  fields::image.plot(
    zlim = c(qmin, qmax), col = qcols(9), legend.only = TRUE,
    legend.width = 4, legend.mar = 1,
    legend.shrink = 1.0,
    bigplot = c(0.1, 0.2, 0.1, 0.2),
    legend.args = list(text = ylabelq, side = 2, line = 0.5, cex = 0.90)
  )

  #################################################################  panel three (element #ten)

  graphics::frame()
  graphics::frame()
  graphics::frame()
  graphics::frame()
  graphics::frame()

  graphics::par(mar = c(1, 0, 0, 1))
  leg.txt <- c(" (A) Partial", " (B) Ice", " (D) Dry", " (E) Estimate")
  lcol <- c("green", "cyan", "yellow", "red")
  graphics::legend("left", leg.txt, pch = c(22, 22, 22, 22), pt.bg = lcol, cex = 1.25, bty = "n")


  ##############################################################  Add title
  tscale <- 1.7
  if (nchar(title) >= 45) tscale <- 1.5
  if (nchar(title) >= 50) tscale <- 1.2
  graphics::mtext(title, side = 3, line = 0, cex = tscale, outer = TRUE)


  ############################################################### end plotting section

}
