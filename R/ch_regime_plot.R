#' Plots the regime of daily streamflows using quantiles
#'
#' @description Produces a regime hydrograph similar to that in the reference. It shows the flow quantiles for each
#' day of the year and the maximum and minimum. Parameters can be set to change colours and set the y-scale
#' to allow plots of same scale to be produced.
#'
#'
#' @param DF  data frame of daily flow data
#' @param quant quantiles; default is \code{quant = c(0.95,0.9,0.75,0.5,0.25,0.1,0.05)}. 
#' Can be changed but the length must be 7 and the 4th value must be 0.5 (median)
#' @param wyear set \code{wyear = 10} for October, \code{water year = 1} for calendar year, can be any month
#' @param colour if \code{TRUE} plot is in colour, if \code{FALSE} plot is grayscale.
#' @param mx set the maximum y value; if = 1 then maximum value of the flows is used to set 
#' @param metadata a data frame of metadata, defaults to HYDAT_list.
#' the y-axis value. The value of \code{mx} can be specified to produce a series of plots with the 
#' same scale.
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
#' data(CAN05AA008)
#' ch_regime_plot(CAN05AA008, colour = TRUE, wyear = 1)
#'

ch_regime_plot <- function(DF, wyear = 1, colour = TRUE, mx = 1, metadata = NULL,
                        quant = c(0.95, 0.9, 0.75, 0.5, 0.25, 0.1, 0.05))
  {
 
  station <- DF[1, 1]
  sname <- ch_get_wscstation(station, metadata)
  title <- sname$Station_lname
  
  ############################################################################# labels
  dmf <- expression(paste("Mean Daily Discharge m("^{3}, "/sec)"))

  flow <- DF$Flow
  Date <- DF$Date
 
  doy_vals <- ch_doys(Date, water_yr = wyear)
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
  if (nchar(title) >= 45) tscale <- 1.0
  if (nchar(title) >= 50) tscale <- 0.8
  
  # capture plotting parameters, restore on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(las = 1)
  par(mar = c(3,5,3,1))
  plot(doy1, regime[9,], type = "p", xlab = "", xaxt = "n", col = colr[4],
       cex = 0.5, ylab = dmf, ylim = ylims, xlim = c(1, 366), 
       main = title, cex.main = tscale)
  ch_axis_doy(wyear)
  polygon(mdays, poly1, col = colr[1], border = colr[1])
  polygon(mdays, poly2, col = colr[2], border = colr[2])
  polygon(mdays, poly3, col = colr[3], border = colr[3])
  points(doy1, regime[1, ], type = "p", col = colr[4], cex = 0.5)
  points(doy1, regime[5, ], type = "l", col = colr[5], lwd = 3)
  
  ltext1 <- c("min / max",
              paste(format( quant[7], nsmall = 2),"-", format(quant[1],nsmall = 2), sep = ""),
              paste(format( quant[6], nsmall = 2),"-", format(quant[2],nsmall = 2), sep = ""),
              paste(format( quant[5], nsmall = 2),"-", format(quant[3],nsmall = 2), sep = ""),
              "median")
  
  lcol1 <- c(colr[4],colr[1],colr[2], colr[3],colr[5])
  legend("topleft", legend = ltext1, col = lcol1, lty = 1, lwd = 3, bty = "n")
  ######################################################################### plot end
}
