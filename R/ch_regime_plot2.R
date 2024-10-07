#' Plots the regime of daily streamflows using quantiles
#'
#' @description Produces a regime hydrograph similar to that in the reference. It shows the flow quantiles for each
#' day of the year and the maximum and minimum. Parameters can be set to change colours and set the y-scale
#' to allow plots of same scale to be produced.
#'
#' @param date,flow Vectors of dates and flow data, or names of the columns
#' containing such data in `data`.
#' @param data Optional data frame containing the data.
#' @param id Gauge code used for creating a smart plot title; could also
#' be a column name in `data`. If `NULL` (the default), title creation
#' is bypassed.
#' @param quant quantiles; default is \code{quant = c(0.95,0.9,0.75,0.5,0.25,0.1,0.05)}. 
#' Can be changed but the length must be 7 and the 4th value must be 0.5 (median)
#' @param wyear set \code{wyear = 10} for October, \code{water year = 1} for calendar year, can be any month
#' @param colour if \code{TRUE} plot is in colour, if \code{FALSE} plot is grayscale.
#' @param metadata a data frame of metadata to look for gauge code for `id`, 
#' defaults to `HYDAT_list` and is not used if `id` isn't.
#' @param ylab Y axis label.
#' @param ... Other arguments to pass to the `plot()` function. Will take
#' precedence over defaults.
#' 
#' @return No value is returned; a standard \R graphic is created.
#' @author Paul Whitfield
#' @importFrom graphics par points polygon legend
#' @importFrom stats quantile
#' @export
#'
#' @references MacCulloch, G. and P. H. Whitfield (2012). Towards a Stream Classification System 
#' for the Canadian Prairie Provinces. Canadian Water Resources Journal 37: 311-332.
#'
#' @examples
#' # Refer to columns in the `CAN05AA008` dataset:
#' ch_regime_plot2(Date, Flow, data = CAN05AA008, id = ID)
#' 
#' # Optionally, can insert vectors.
#' x <- CAN05AA008$Date
#' y <- CAN05AA008$Flow
#' ch_regime_plot2(x, y)
#' 
#' # Override `plot()` defaults using ...; for instance, zero-in on the freshet.
#' ch_regime_plot2(x, y, xlim = c(90, 220))
ch_regime_plot2 <- function(
    date, flow, data = NULL, id = NULL, wyear = 1, colour = TRUE,
    metadata = HYDAT_list, 
    ylab = expression(paste("Mean Daily Discharge (", m^3, "/sec)")),
    quant = c(0.95, 0.9, 0.75, 0.5, 0.25, 0.1, 0.05), ...)
{  
  flow <- rlang::eval_tidy(rlang::enquo(flow), data = data)
  date <- rlang::eval_tidy(rlang::enquo(date), data = data)
  v <- vctrs::vec_recycle_common(flow, date)
  flow <- v[[1]]
  date <- v[[2]]
  
  id <- unique(rlang::eval_tidy(rlang::enquo(id), data = data))
  
  if (is.null(id)) {
    title <- NULL
  } else {
    if (length(id) > 1) {
      stop("Received more than one station ID: ", paste0(id, collapse = ", "))
    }
    if (length(id) == 0) {
      stop("`id` did not evaluate to any gauge code.")
    }
    sname <- ch_get_wscstation(id, metadata)
    title <- sname$Station_lname
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
    cex = 0.5, ylab = ylab, xlim = c(1, 366), 
    main = title, cex.main = tscale,
    ylim = c(0, max(flow, na.rm = TRUE))
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
    paste(
      format(quant[7], nsmall = 2), "-", format(quant[1], nsmall = 2), sep = ""
    ),
    paste(
      format(quant[6], nsmall = 2), "-", format(quant[2], nsmall = 2), sep = ""
    ),
    paste(
      format(quant[5], nsmall = 2), "-", format(quant[3], nsmall = 2), sep = ""
    ),
    "median"
  )
  
  lcol1 <- c(colr[4], colr[1], colr[2], colr[3], colr[5])
  legend("topleft", legend = ltext1, col = lcol1, lty = 1, lwd = 3, bty = "n")
  ######################################################################### plot end
  invisible()
}
