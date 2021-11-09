#' Create Booth plot of peaks over a threshold
#'
#' A Booth plot is a plot of peaks over threshold flood events with duration on the horizontal and
#' either magnitude (default) or volume on the vertical axis.
#'
#' @param events A data frame of POT events from the function \code{ch_get_peaks}
#' @param threshold The threshold used by \code{ch_get_peaks}
#' @param title Plot title
#' @param type The plot type, either \option{mag} (magnitude, the default) or \option{vol} (volume)
#' @param colour1 A vector of length 12 with line colours of rings or symbols. Defaults to those used by Booth.
#' @param colour2 A vector of length 12 with fill colours of rings or symbols. Defaults to those used by Booth.
#' @author Paul Whitfield 
#' 
#' @references
#' Booth, E.G., Mount, J.F., Viers, J.H. 2006. Hydrologic Variability of the Cosumnes River Floodplain.
#' San Francisco Estuary & Watershed Science 4:21.
#'
#' Whitfield, P.H., and J.W. Pomeroy. 2016. Changes to flood peaks of a mountain river: implications
#' for analysis of the 2013 flood in the Upper Bow River, Canada. Hydrological Processes 30:4657-73. doi:
#' 10.1002/hyp.10957.
#' @importFrom graphics axis legend par plot points polygon abline
#' @export
#' @return No value is returned; a standard \R graphic is created.
#' @keywords plot
#' @seealso \code{\link{ch_get_peaks}}
#' @examples
#' threshold <- 0.1 * max(CAN05AA008$Flow)  # arbitrary threshold
#' peaks <- ch_get_peaks(CAN05AA008, threshold)
#' events <- peaks$POTevents
#' ch_booth_plot(events, threshold, title = "05AA008", type='mag')
#' ch_booth_plot(events, threshold, title = "05AA008", type='vol')

ch_booth_plot <- function(events, threshold, title, type = "mag", colour1 = 1, colour2 = 1) {

  # set common items
  mname <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ocol <- c("black", "blue", "darkgreen", "black", "blue", "darkgreen", "black", "blue", "darkgreen", "black", "blue", "darkgreen")
  mcol <- c("gray10", "blue", "slateblue3", "slateblue4", "green", "cyan", "green4", "darkorange", "red", "darkorange4", "gray70", "gray40")

  if (length(colour1) == 12) {
    ocol <- colour1
  } else {
    message(paste("length of colour1 is", length(colour1), " must be of length 12; Using defaults"))
  }

  if (length(colour2) == 12) {
    mcol <- colour2
  } else {
    message(paste("length of colour2 is", length(colour2), " must be of length 12;Using defaults"))
  }

  xlabel <- "Duration (days)"
  xlines <- c(7, 14, 21, 60)
  xlimits <- c(1, 350)

  vlabel <- expression(paste("Event volume km"^{3}))
  vlines <- c(.01, .02, .05, .1, .2, .5, 1., 2., 5., 10., 20., 50., 100., 200., 500., 1000., 2000., 5000., 10000.)

  ylabel <- expression(paste("Mean Daily Discharge m"^{3}, "/sec"))
  ylines <- c(.1, .2, .5, 1., 2., 5., 10., 20., 50., 100., 200., 500., 1000., 2000., 5000., 10000.)

  month <- as.numeric(format(events$st_date, "%m"))

  ############################################################################   for volume
  if (type == "vol") {
    ylimits <- c(min(events[, 4], na.rm = TRUE), round(max(events[, 4], na.rm = TRUE), digits = 1))

    plot(events[, 5], events[, 4],
      xlab = xlabel, col = ocol[month], bg = mcol[month], pch = 22, xlim = xlimits, ylim = ylimits, ylab = vlabel,
      yaxt = "n", log = "xy", main = title
    )
    abline(h = vlines, lty = 3, col = "gray50")
    abline(v = xlines, lty = 3, col = "gray50")
    axis(2, las = 2)
    legend("topright", mname, pch = 22, col = ocol, pt.bg = mcol, bg = "white")
    mtext(paste("Threshold=", threshold, " m3/s"), side = 4, line = 1)
  }

  ############################################################################ for magnitude
  if (type == "mag") {
    ylimits <- c(threshold, round(max(events[, 3], na.rm = TRUE), digits = 0))

    plot(events[, 5], events[, 3],
      xlab = xlabel, col = ocol[month], bg = mcol[month], pch = 21, cex = 1.1, xlim = xlimits, ylim = ylimits, ylab = ylabel,
      yaxt = "n", log = "xy", main = title
    )
    abline(h = ylines, lty = 3, col = "gray50")
    abline(v = xlines, lty = 3, col = "gray50")
    axis(2, las = 2)
    legend("topright", mname, pch = 21, col = ocol, pt.bg = mcol, bg = "white")
    mtext(paste("Threshold=", threshold, " m3/s"), side = 4, line = 1)
  }
  ############################################################################
}
