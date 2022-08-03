#' @title Hydrograph plot
#'
#' @description 
#' Creates a hydrograph plot for simulated, observed, and inflow
#' hydrograph series, including precipitation if provided. The secondary y axis 
#' will be used to plot the precip time series.
#' 
#' @details
#' Assumes that the supplied time series have the same length and
#' duration in time. If this is not true, then the defined period or period
#' calculated from the first available flow series will be used to determine
#' the plotting limits in time. The supplied time series should be in \pkg{xts} format.
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param flows data frame of flows to plot
#' @param precip data frame of precipitation values to plot
#' @param prd period to use in plotting
#' @param winter_shading optionally adds a transparent cyan shading for the
#' December 1st to March 31st period in each year that is plotted. Default is
#' \code{FALSE}.
#' @param winter_colour colour to use in winter shading polygons
#' @param range_mult_flow range multiplier for max value in hydrograph. This is 
#' useful in preventing overlap if precip is also plotted. This value should not 
#' be less than 1.0, otherwise the values will be
#' cutoff in the plot.
#' @param range_mult_precip range multiplier for max value in precipitation plot (default 1.5)
#' @param flow_labels string vector of labels for flow values
#' @param ylabel text label for y-axis of the plot (default 'Flow [m^3/s]')
#' @param precip_label text label for precipitation y-axis (default 'Precipitation [mm]')
#' @param leg_pos string specifying legend placement on plot e.g. \option{topleft},
#' \option{right}, etc., and is consistent with the legend function options. If \code{NULL}, 
#' the function will place the legend left, if precip added, on the topleft
#' otherwise).
#' @param leg_box boolean on whether to put legend in an opaque white box
#' or not. If \code{NULL} (the default), the function will automatically not use a white box
#' and leave the background of the legend transparent.
#' @param zero_axis fixes the y axis to start exactly at zero (default \code{TRUE}). 
#' By default, R will plot the values with a
#' small buffer for presentation. Be warned that if this option is set to
#' TRUE, the minimum value is set to zero without checking if any flow values
#' are less than zero. This option should not be used for reservoir stage plotting, since
#' most reservoir stage is typically reported as an elevation.
#' 
#' @return Returns \code{TRUE} if the function is executed properly.
#' 
#' @author Robert Chlumsky
#' 
#' @examples
#' # example with synthetic random data
#' dd <- seq.Date(as.Date("2010-10-01"), as.Date("2013-09-30"),by = 1)
#' x <- abs(rnorm(length(dd)))
#' y <- abs(rnorm(length(dd))) * x
#' df <- data.frame("Date" = dd, x, y)
#' myprd <- "2011-10-01/2012-09-30"
#'
#' precip <- data.frame("Date" = dd," precip" = abs(rnorm(length(dd))) * 10)
#'
#' # basic hydrograph plot
#' ch_hydrograph_plot(flows = df, winter_shading = FALSE)
#'
#' # with different labels and winter shading
#' ch_hydrograph_plot(flows = df, winter_shading = TRUE,
#'  flow_labels = c("simulated", "observed"))
#'
#' # add precipitation, increase the plot ranges to separate flows and precip, and add a legend box
#' ch_hydrograph_plot(flows = df, precip = precip, range_mult_flow = 1.7, 
#' range_mult_precip = 2, leg_box = TRUE)
#' 
#' @importFrom lubridate year month day date
#' @importFrom graphics grid lines
#' @export
#' 
ch_hydrograph_plot <- function(flows = NULL, 
                            precip = NULL, 
                            prd = NULL, 
                            winter_shading = FALSE, 
                            winter_colour='cyan',
                            range_mult_flow = NULL, 
                            range_mult_precip = 1.5,
                            flow_labels = NULL, 
                            ylabel = NULL,
                            precip_label = "Precipitation [mm]",
                            leg_pos = NULL, 
                            leg_box = NULL, 
                            zero_axis = TRUE) {

  # check flows data frame
  if (!(is.null(flows))) {
    if (!inherits(flows, "data.frame")) {  
      stop("flows must be a data frame.")
    }
    if (nrow(flows) == 0) {
      stop("flows data frame cannot be empty (zero rows).")
    }
    if (ncol(flows) == 1) {
      stop("flows data frame cannot be empty (no data columns).")
    }
    if (which(colnames(flows) == "Date") != 1) {
      stop("'Date' must be the first attribute of flows data frame.")
    }
    if (is.null(flows$Date)) {
      stop("Date attribute is required in flows data frame.")
    }
    if (ncol(flows) > 11) {
      stop("flows cannot have more than 11 data columns (other than 'Date').")
    }
  }
  
  # check flow labels
  if (!(is.null(flow_labels))) {
    if (length(flow_labels) != ncol(flows) - 1) {
      stop("flow_labels must have the same number of labels as the flows data frame (not including Date attribute).")
    }
  } else {
    flow_labels <- colnames(flows)[2:ncol(flows)]
  }
  
  # check ylabel
  if (is.null(ylabel)) {
    ylabel <- expression(paste("Flow [m" ^{3}, "/s]"))
  }

  # check precip data frame
  if (!(is.null(precip))) {
    if (!inherits(precip, "data.frame")) {  
      stop("precip must be a data frame.")
    }
    if (nrow(precip) == 0) {
      stop("precip data frame cannot be empty (zero rows).")
    }
    if (ncol(precip) == 1) {
      stop("precip data frame cannot be empty (no data columns).")
    }
    if (which(colnames(precip) == "Date") != 1) {
      stop("'Date' must be the first attribute of precip data frame.")
    }
    if (is.null(precip$Date)) {
      stop("Date attribute is required in precip data frame.")
    }
    if (ncol(precip) > 2) {
      stop("precip cannot have more than 1 data column (other than 'Date').")
    }
  }

  # check range.mult input
  if (!(is.null(range_mult_flow))) {
    if (range_mult_flow <= 0) {
      stop("range_mult_flow must be a positive value.")
    }
    if (range_mult_flow < 1) {
      warning("range_mult_flow is less than one, plot may be cut off.")
    }
  }
  if (!(is.na(range_mult_precip))) {
    if (range_mult_precip <= 0) {
      stop("range_mult_precip must be a positive value.")
    }
    if (range_mult_precip < 1) {
      warning("range_mult_precip is less than one, plot may be cut off.")
    }
  }

  # adjust range_mult_flow if precip is NULL
  if (is.null(range_mult_flow)) {
    if (is.null(precip)) {
      range_mult_flow <- 1.05
    } else {
      range_mult_flow <- 1.5
    }
  }

  # determine period ----
  if (!(is.null(prd))) {

    # period is supplied; check that it makes sense
    firstsplit <- unlist(strsplit(prd, "/"))
    if (length(firstsplit) != 2) {
      stop("Check the format of supplied period; should be two dates separated by '/'.")
    }
    if (length(unlist(strsplit(firstsplit[1], "-"))) != 3 || length(unlist(strsplit(firstsplit[2], "-"))) != 3
    || nchar(firstsplit[1]) != 10 || nchar(firstsplit[2]) != 10) {
      stop("Check the format of supplied period; two dates should be in YYYY-MM-DD format.")
    }
    if (nrow(ch_date_subset(flows, prd)) == 0) {
      stop("prd does not overlap with flows; check prd and flows data frame.")
    }
  } else {
    # period is not supplied

    # define entire range as period
    N <- nrow(flows)
    prd <- sprintf(
      "%d-%02d-%02d/%i-%02d-%02d", 
      year(flows$Date[1]), 
      month(flows$Date[1]), 
      day(flows$Date[1]),
      year(flows$Date[N]), 
      month(flows$Date[N]), 
      day(flows$Date[N])
    )
  }

  # subset data
  flows <- ch_date_subset(flows, prd)

  if (!(is.null(precip))) {
    precip <- ch_date_subset(precip, prd)
    if (nrow(precip) == 0) {
      warning("precip data does not overlap with prd; check data and prd arguments.")
      precip <- NULL
    }
  }

  # capture plotting parameters, restore on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  # set parameters for plotting; then plot
  if (!(is.null(precip))) {
    par(mar = c(5, 4, 4, 4) + 0.1)
  }
  if (zero_axis) {
    # sets the interval calculation in plotting to be right to specified limits
    # otherwise extends by 4% by default
    par(yaxs = "i")
  }

  y.hmax <- max(flows[, 2:ncol(flows)], na.rm = TRUE) * range_mult_flow

  if (zero_axis) {
    y.hmin <- 0
  } else {
    y.hmin <- min(flows[, 2:(ncol(flows))], na.rm = TRUE)
  }

  plot(flows$Date, flows[, 2],
    xlab = "Date", ylab = ylabel,
    col = "white", type = "l", ylim = c(y.hmin, y.hmax), panel.first = grid()
  )
  if (winter_shading) {
    # shaded winter months
    temp <- flows[((month(flows$Date) == 12) & (day(flows$Date) == 1)) |
      ((month(flows$Date) == 3) & (day(flows$Date) == 31)), ]
    ep <- match(date(temp$Date), date(flows$Date))
    if (month(flows$Date[ep[1]]) == 3) {
      # ep <- ep[-1]
      ep <- c(1, ep)
    }
    if (month(flows$Date[ep[length(ep)]]) == 12) {
      # ep <- ep[-length(ep)]
      ep <- c(ep, nrow(flows))
    }
    # bc <- "#FF0000E6" #  "#00FFFF32"
    for (k in seq(1, length(ep), 2)) {
      cord.x <- c(
        date(flows$Date[ep[k]]), date(flows$Date[ep[k]]),
        date(flows$Date[ep[k + 1]]), date(flows$Date[ep[k + 1]])
      )
      cord.y <- c(-1e3, y.hmax * 1e3, y.hmax * 1e3, -1e3)
      polygon(cord.x, cord.y, col = winter_colour, border = NA)
    }
  }

  # define legend items
  NN <- ncol(flows) - 1
  leg.items <- flow_labels
  # want to replace colour selection with a nice package that picks nice colours together, for any number of inputs
  leg.cols <- c("red", "navyblue", "black", "orange", "cyan", "darkgreen", "coral1", "deeppink", "blue", "orangered3")[1:NN]
  leg.lty <- rep(seq(1, 5, 1), 3)[1:NN]
  leg.lwd <- rep(1, NN)

  # add all flow data to plot
  for (i in 1:NN) {
    lines(flows$Date, flows[, (i + 1)], lty = leg.lty[i], lwd = leg.lwd[i], col = leg.cols[i])
  }

  # add precip data if not null
  if (!(is.null(precip))) {
    par(new = TRUE)
    precip.col <- "#0000FF64"
    plot(precip$Date, precip[, 2],
      col = precip.col, lty = 1, lwd = 1,
      type = "h", ylim = rev(c(0, max(precip[, 2], na.rm = TRUE) * range_mult_precip)), xaxt = "n", yaxt = "n",
      xlab = "", ylab = ""
    )
    axis(4)
    mtext(sprintf("%s", precip_label), side = 4, line = 2.5)

    leg.items <- c(leg.items, precip_label)
    leg.cols <- c(leg.cols, precip.col)
    leg.lty <- c(leg.lty, 1)
    leg.lwd <- c(leg.lwd, 1)
  }

  if (is.null(leg_pos)) {
    if (!(is.null(precip))) {
      leg_pos <- "left"
    } else {
      leg_pos <- "topleft"
    }
  }
  if (is.null(leg_box)) {
    leg_box <- "n"
  } else {
    if (leg_box) {
      leg_box <- "o"
    } else {
      leg_box <- "n"
    }
  }

  # add legend to plot
  legend(
    x = leg_pos, 
    legend = leg.items, 
    lty = leg.lty, 
    col = leg.cols,
    lwd = leg.lwd, 
    bty = leg_box, 
    cex = 0.8, inset = 0.01
  )

  return(TRUE)
}
