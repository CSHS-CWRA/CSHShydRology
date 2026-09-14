#' Plots the regime of daily streamflows using quantiles
#'
#' @description Produces a regime hydrograph similar to that in the reference. It shows the flow quantiles for each
#' day of the year and the maximum and minimum. Parameters can be set to change colours and set the y-scale
#' to allow plots of same scale to be produced.
#'
#' @details The function accepts data in two ways.
#'
#' By default it takes a data frame of daily flows as its first argument, which
#' must contain \code{Date} and \code{Flow} columns, with the station code in
#' the first column. This is the original interface and is unchanged:
#'
#' \code{ch_regime_plot(CAN05AA008, colour = TRUE, wyear = 1)}
#'
#' Alternatively, name the columns to use. They are looked up in \code{DF},
#' which lets the function work with data frames that are not in the layout
#' above:
#'
#' \code{ch_regime_plot(CAN05AA008, date = Date, flow = Flow, id = ID)}
#'
#' Or leave \code{DF} unset and pass vectors, from any source at all:
#'
#' \code{ch_regime_plot(date = my_dates, flow = my_flows)}
#'
#' The arguments of the second interface come after those of the first, so any
#' call written for the original interface behaves identically.
#'
#' @param DF data frame of daily flow data. Used on its own, it must contain
#' \code{Date} and \code{Flow} columns, with the station code in the first
#' column. It is also the place \code{date}, \code{flow} and \code{id} are
#' looked up when those are given. Leave as \code{NULL} to supply vectors.
#' @param quant quantiles; default is \code{quant = c(0.95,0.9,0.75,0.5,0.25,0.1,0.05)}. 
#' Can be changed but the length must be 7 and the 4th value must be 0.5 (median)
#' @param wyear set \code{wyear = 10} for October, \code{water year = 1} for calendar year, can be any month
#' @param colour if \code{TRUE} plot is in colour, if \code{FALSE} plot is grayscale.
#' @param mx set the maximum y value; if = 1 then maximum value of the flows is used to set 
#' the y-axis value. The value of \code{mx} can be specified to produce a series of plots with the 
#' same scale.
#' @param metadata a data frame of metadata in which to look up the station. If
#' \code{NULL} (the default), \code{HYDAT_list} is used.
#' @param date Vector of dates, or the name of the column in \code{DF} holding
#' them (unquoted). Supply with \code{flow} instead of relying on the
#' \code{Date} and \code{Flow} columns of \code{DF}.
#' @param flow Vector of flows, or the name of the column in \code{DF} holding
#' them (unquoted).
#' @param id Gauge code, used to build the plot title; may also be the name of a
#' column in \code{DF}. If \code{NULL} (the default) no title is created.
#' Only used alongside \code{date} and \code{flow}.
#' @param ... Other arguments to pass to the \code{plot()} function. These take
#' precedence over the defaults set by this function, so passing \code{ylab},
#' \code{xlim} or \code{main} here overrides the default that would otherwise
#' be used.
#' 
#' @return No value is returned; a standard \R graphic is created.
#' @author Paul Whitfield, Vincenzo Coia
#' @importFrom graphics par points polygon legend
#' @importFrom stats quantile
#' @export
#'
#' @references MacCulloch, G. and P. H. Whitfield (2012). Towards a Stream Classification System 
#' for the Canadian Prairie Provinces. Canadian Water Resources Journal 37: 311-332.
#'
#' @examples
#' data(CAN05AA008)
#' ch_regime_plot(CAN05AA008, colour = TRUE, wyear = 1)
#'
#' # Name the columns to use, for data frames not in the layout above.
#' ch_regime_plot(CAN05AA008, date = Date, flow = Flow, id = ID)
#'
#' # Or supply vectors, from any source.
#' ch_regime_plot(date = CAN05AA008$Date, flow = CAN05AA008$Flow)
#'
#' # Override plot() defaults through ...; for instance, zoom in on the freshet.
#' ch_regime_plot(CAN05AA008, date = Date, flow = Flow, xlim = c(90, 220))
ch_regime_plot <- function(DF = NULL,
                           wyear = 1,
                           colour = TRUE,
                           mx = 1,
                           metadata = NULL,
                           quant = c(0.95, 0.9, 0.75, 0.5, 0.25, 0.1, 0.05),
                           date = NULL,
                           flow = NULL,
                           id = NULL,
                           ...) {
  q_date <- rlang::enquo(date)
  q_flow <- rlang::enquo(flow)
  q_id <- rlang::enquo(id)

  if (!rlang::quo_is_null(q_date) || !rlang::quo_is_null(q_flow)) {
    # ------------------- `date` and `flow` given; DF, if any, is the data source
    if (rlang::quo_is_null(q_date) || rlang::quo_is_null(q_flow)) {
      stop("Supply both `date` and `flow`.", call. = FALSE)
    }
    if (!is.null(DF) && !is.data.frame(DF)) {
      stop("`DF` must be a data frame. To supply vectors, use `date` and `flow`.",
           call. = FALSE)
    }
    date <- rlang::eval_tidy(q_date, data = DF)
    flow <- rlang::eval_tidy(q_flow, data = DF)
    id <- unique(rlang::eval_tidy(q_id, data = DF))
  } else {
    # ------------------------------------------------- original interface
    if (is.null(DF)) {
      stop("Supply either `DF`, or both `date` and `flow`.", call. = FALSE)
    }
    if (!is.data.frame(DF)) {
      stop("`DF` must be a data frame. To supply vectors, use `date` and `flow`.",
           call. = FALSE)
    }
    if (!all(c("Date", "Flow") %in% names(DF))) {
      stop("`DF` must have `Date` and `Flow` columns.", call. = FALSE)
    }
    date <- DF$Date
    flow <- DF$Flow
    id <- DF[1, 1]
  }

  v <- vctrs::vec_recycle_common(flow, date)
  flow <- v[[1]]
  date <- v[[2]]

  if (is.null(id)) {
    title <- NULL
  } else {
    if (length(id) > 1) {
      stop("Received more than one station ID: ", paste0(id, collapse = ", "),
           call. = FALSE)
    }
    if (length(id) == 0) {
      stop("`id` did not evaluate to any gauge code.", call. = FALSE)
    }
    sname <- ch_get_wscstation(id, metadata)
    title <- if (is.data.frame(sname)) sname$Station_lname else NULL
  }

  ############################################################################# labels
  doy_vals <- ch_doys(date, water_yr = wyear)
  year <- doy_vals$year
  doy <- doy_vals$doy

  if (wyear != 1)  doy <- doy_vals$dwy

  doys <- 366
  doy1 <- c(1:doys)
  years <- unique(year)
  nyears <- max(years) - min(years) + 1
  min_year <- min(years) - 1

  ############################################################################# arrays
  q <- array(NA, dim = c(nyears, doys))

  colr <- c("gray70", "gray50", "gray30", "black", "gray10")
  if (colour == TRUE) colr <- c("gray", "cyan", "deepskyblue2", "red", "darkblue")

  ########################################################################## create table of year of daily discharge
  for (k in 1:length(year)) {
    q[(year[k] - min_year), doy[k]] <- flow[k]
  }

  qquantiles <- quant
  qquantiles <- rev(qquantiles)

  regime <- array(NA, dim = c(9, doys))

  for (jj in 1:doys) {
    regime[1, jj] <- min(q[, jj], na.rm = TRUE)
    regime[9, jj] <- max(q[, jj], na.rm = TRUE)
    for (j in 2:8) {
      regime[j, jj] <- stats::quantile(q[, jj], probs = qquantiles[j - 1], na.rm = TRUE)
    }
  }

  ############################  need to replace Inf and -Inf with NA  Infs come from all days being NA
  regime[is.infinite(regime)] <- NA

  ###########################  create polygons for 0.95-0.05, 0.90-0.1. 0.75-0.25
  ylims <- c(0, mx)
  if (mx == 1) ylims <- c(0, max(flow, na.rm = TRUE))

  mdays <- c(doy1, rev(doy1))
  poly1 <- c(regime[2, ], rev(regime[8, ]))
  poly2 <- c(regime[3, ], rev(regime[7, ]))
  poly3 <- c(regime[4, ], rev(regime[6, ]))

  ######################################################################### plot start
  tscale <- 1.2
  if (!is.null(title) && nchar(title) >= 45) tscale <- 1.0
  if (!is.null(title) && nchar(title) >= 50) tscale <- 0.8

  # capture plotting parameters, restore on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(las = 1)
  par(mar = c(3,5,3,1))

  # Allow override of default plot options by evaluating in a data mask.
  plot_defaults <- rlang::env(
    xlab = "", xaxt = "n", col = colr[4],
    cex = 0.5, xlim = c(1, 366),
    ylab = expression(paste("Mean Daily Discharge m("^{3}, "/sec)")),
    main = title, cex.main = tscale,
    ylim = ylims
  )
  dots <- rlang::list2(...)
  all_args <- union(names(plot_defaults), names(dots))
  names(all_args) <- all_args
  all_args <- rlang::parse_exprs(all_args)
  all_args_eval <- lapply(all_args, \(x) {
    rlang::eval_tidy(x, data = dots, env = plot_defaults)
  })

  # Plot with reconciled arguments
  rlang::exec("plot", doy1, regime[9, ], type = "p", !!!all_args_eval)
  ch_axis_doy(wyear)
  polygon(mdays, poly1, col = colr[1], border = colr[1])
  polygon(mdays, poly2, col = colr[2], border = colr[2])
  polygon(mdays, poly3, col = colr[3], border = colr[3])
  points(doy1, regime[1, ], type = "p", col = colr[4], cex = 0.5)
  points(doy1, regime[5, ], type = "l", col = colr[5], lwd = 3)

  ltext1 <- c(
    "min / max",
    paste(format(quant[7], nsmall = 2), "-", format(quant[1], nsmall = 2), sep = ""),
    paste(format(quant[6], nsmall = 2), "-", format(quant[2], nsmall = 2), sep = ""),
    paste(format(quant[5], nsmall = 2), "-", format(quant[3], nsmall = 2), sep = ""),
    "median"
  )

  lcol1 <- c(colr[4], colr[1], colr[2], colr[3], colr[5])
  legend("topleft", legend = ltext1, col = lcol1, lty = 1, lwd = 3, bty = "n")
  ######################################################################### plot end
  invisible()
}
