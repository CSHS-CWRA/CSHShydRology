#' Polar plot of daily streamflows
#'
#' @description Produces a polar plot similar to that used in \cite{Whitfield and Cannon, 2000}. It uses output 
#' from the function \code{\link{ch_binned_MannWhitney}} or a data structure created using 
#' the function \code{\link{ch_polar_plot_prep}}.
#'
#' @param bmw output from \code{\link{ch_binned_MannWhitney}}
#' @param lcol1 line colour, default is \code{c("black","gray50")}
#' @param lcol2 point colour, default is \code{c("black","gray50")}
#' @param lfill fill colour, default is \code{c("yellow","green")}
#' @param lsig significance symbol colour, default is \code{c("red","blue")}
#'
#' @references
#' Whitfield, P.H. and A.J. Cannon. 2000. Polar plotting of seasonal hydrologic
#' and climatic data. Northwest Science 74: 76-80.
#'
#' Whitfield, P.H., Cannon, A.J., 2000. Recent variations in climate and hydrology
#' in Canada. Canadian Water Resources Journal 25: 19-65.
#' @return No value is returned; a standard \R graphic is created.
#' @keywords plot
#' @author Paul Whitfield
#' @importFrom plotrix radial.plot radial.grid
#' @importFrom stats approx
#' @export
#' @seealso \code{\link{ch_binned_MannWhitney}} \code{\link{ch_polar_plot_prep}}

#' @examples 
#' range1 <- c(1970,1979)
#' range2 <- c(1990,1999)
#' b_MW <- ch_binned_MannWhitney(CAN05AA008, step = 5, range1, range2, 
#' ptest <- 0.05)
#' ch_polar_plot(b_MW)

ch_polar_plot <- function(bmw, lcol1 = c("black", "gray50"), lcol2 = c("black", "gray50"),
                       lfill = c("yellow", "green"), lsig = c("red", "blue")) {
  dlabels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  dbreaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  dbreaks <- dbreaks / 365 * 2 * pi

  series <- bmw$series
  llines <- array(NA, dim = 2)
  llines[1] <- paste(bmw$range1[1], "-", bmw$range1[2], bmw$bin_method)
  llines[2] <- paste(bmw$range2[1], "-", bmw$range2[2], bmw$bin_method)

  bins <- length(series[, 1])
  cpos <- c(1:bins)
  cpos <- cpos / bins * 365
  cpos <- cpos / 365 * 2 * pi
  cpos <- cpos[1:length(series[, 2])]

  rlim <- c(0, max(series[, 2], series[, 3]) * 1.01)
  rlim[1] <- -rlim[2] / 6
  mdelta <- (rlim[2] - rlim[1]) / 20

  xmax <- array(data = NA, dim = length(series[, 1]))
  xmin <- array(data = NA, dim = length(series[, 1]))
  
  # capture plotting parameters, restore on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mfrow = c(1, 1))

  #  set basic polar plot be plotting first series which will have a radial line for each interval in set
  par(cex.lab = 0.75)
  radial.plot(as.numeric(series[, 2]), cpos,
    rp.type = "ps",
    line.col = lcol1[1], point.symbols = 16, point.col = lcol2[1],
    labels = NULL, label.pos = cpos,
    radial.lim = rlim, show.grid.labels = 1,
    start = 3 * pi / 2, clockwise = TRUE,
    main = bmw$Station_lname
  )
  par(cex.lab = 1.0)
  # add the polygons for periods of increase and decreases

  # create the polygons for increases and decreases

  ppolys <- data.frame(cpos, as.numeric(series[, 2]), as.numeric(series[, 3]))

  # add a row to span back to the first element
  ppolys[bins + 1, ] <- ppolys[1, ]
  ppolys[bins + 1, 1] <- ppolys[bins + 1, 1] + ppolys[bins, 1] # adjust so radians continue

  p1 <- approx(ppolys[, 1], n = 10 * (bins + 1))
  p2 <- approx(ppolys[, 2], n = 10 * (bins + 1))
  p3 <- approx(ppolys[, 3], n = 10 * (bins + 1))

  pp <- data.frame(p1[1], p1[2], p2[2], p3[2])
  pp[, 5] <- pp[, 3] > pp[, 4] # add a column of p1 greater than p2

  intersect.points <- which(diff(pp[, 5]) != 0)
  ipoints <- c(1, intersect.points, length(pp[, 1]))



  for (j in 2:length(ipoints)) {
    polyx <- c(
      pp[ipoints[j - 1]:ipoints[j], 2],
      rev(pp[ipoints[j - 1]:ipoints[j], 2])
    )
    polyy <- c(
      pp[ipoints[j - 1]:ipoints[j], 3],
      rev(pp[ipoints[j - 1]:ipoints[j], 4])
    )

    test <- ifelse(pp[ipoints[j], 5], 1, 2)
    radial.plot(polyy, polyx,
      rp.type = "p", poly.col = lfill[test], line.col = NA,
      radial.lim = rlim, cex = 0.5,
      start = 3 * pi / 2, clockwise = TRUE, add = TRUE
    )
  }

  # replot the first data set so it appreas on top of poylgons and then add the second

  radial.plot(as.numeric(series[, 2]), cpos,
    rp.type = "ps", line.col = lcol1[1],
    point.symbols = 16, point.col = lcol2[1],
    radial.lim = rlim, cex = 0.5,
    start = 3 * pi / 2, clockwise = TRUE, add = TRUE
  )

  radial.plot(as.numeric(series[, 3]), cpos,
    rp.type = "ps", line.col = lcol1[2],
    point.symbols = 16, point.col = lcol2[2],
    radial.lim = rlim, cex = 0.5,
    start = 3 * pi / 2, clockwise = TRUE, add = TRUE
  )

  gp <- pretty(rlim)
  radial.grid(
    labels = dlabels, label.pos = dbreaks, radlab = FALSE,
    radial.lim = rlim, clockwise = TRUE,
    start = 3 * pi / 2, grid.col = "gray20", show.radial.grid = FALSE,
    start.plot = FALSE, grid.pos = gp[length(gp)]
  )

  # get positions for significance symbols

  for (k in 1:bins) {
    xmax[k] <- max(series[k, 2], series[k, 3]) + mdelta
    xmin[k] <- min(series[k, 2], series[k, 3]) - mdelta
  }


  # set up for plotting the significance arrows

  ssym <- data.frame(cpos, series[, 6], xmax, xmin)
  names(ssym) <- c("cpos", "t", "xmax", "xmin")

  spsym <- ssym[ssym$t == 1, ]
  snsym <- ssym[ssym$t == -1, ]

  for (k in 1:length(snsym[, 1])) {
    radial.plot(snsym$xmin[k], snsym$cpos[k],
      rp.type = "s", point.symbols = 24, point.col = lsig[1], bg = lsig[1],
      radial.lim = rlim, cex = 0.85,
      start = 3 * pi / 2, clockwise = TRUE, add = TRUE
    )
  }

  for (k in 1:length(spsym[, 1])) {
    radial.plot(spsym$xmax[k], spsym$cpos[k],
      rp.type = "s", point.symbols = 25, point.col = lsig[2], bg = lsig[2],
      radial.lim = rlim, cex = 0.85,
      start = 3 * pi / 2, clockwise = TRUE, add = TRUE
    )
  }

  # add legend
  ltext <- c(
    llines, paste("Decrease in", bmw$variable), paste("Increase in", bmw$variable),
    "Significant Decrease", "Significant Increase", " ", paste("Method", bmw$test_method),
    paste("p<=", bmw$p_used)
  )
  lcols <- c(lcol1, lfill, lsig, "black", "black", "black")
  lcols1 <- c(NA, NA, NA, NA, lsig)
  lsym <- c(19, 19, 15, 15, 25, 24, NA, NA, NA)
  lcex <- c(0.8, 0.8, 1.25, 1.25, 0.9, 0.9, NA, NA, NA)
  lln <- c(1, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  legend(rlim[2] * 1.6, rlim[2] * 1.5, ltext,
    pch = lsym, col = lcols, bty = "n", lty = lln, pt.cex = lcex,
    pt.bg = lcols1, cex = 0.75
  )
}
