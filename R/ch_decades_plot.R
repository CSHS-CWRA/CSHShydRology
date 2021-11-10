#' Plots output from ch_binned_MannWhitney for decades
#'   
#' @description  Creates a simple plot comparing two decades from the 
#' output of \code{ch_binned_MannWhitney}.
#'   
#' @author Paul Whitfield 
#' 
#' @param mplot List output by the function \code{ch_binned_MannWhitney}
#
#' @return A standard \R graphic is created.
#' 
#' @export
#' @seealso \code{\link{ch_decades_plot}}
#' @examples
#' range1 <- c(1970, 1979)
#' range2 <- c(1990, 1999)
#' b_MW <- ch_binned_MannWhitney(CAN05AA008, step = 5, range1, range2, ptest = 0.05)
#' ch_decades_plot(b_MW)

ch_decades_plot <- function(mplot) {
  
  mch <- c(24, NA, 25)
  disch <- expression( paste( "Median Period Discharge (m"^{3}, "/sec)"))
  scol <- c("blue", NA, "red")
  
  series <- mplot$series
  ylims <- c(0, max(series$median_1, series$median_2))
  code <- series$s_code + 2
  
  
  plot(series$period,series$median_1,
       pch = 19, col = "darkblue", type = "b", ylim = ylims, 
       xlab = paste(mplot$bin_width, "-day period", sep = ""),
       main = mplot$Station_lname, ylab = disch)
  points(series$period,series$median_2,
       pch = 1, cex = 1.2, col = "darkgreen", type = "b", lty = 3)
  points(series$period,series$median_2,
       pch = mch[code], cex = 1.25,  col = scol[code], bg = scol[code])
  
  ltext <- c(paste("Median ",mplot$range1[1], "-", mplot$range1[2], sep = ""),
             paste("Median ",mplot$range2[1], "-", mplot$range2[2], sep = ""), 
             "Signiificant Increase", "Significant Decrease")
  lsym <- c(19, 1, 24, 25)
  lcol <- c("darkblue", "darkgreen", "blue", "red")
  legend("topright", legend = ltext, col = lcol, pch = lsym, pt.bg = lcol)
}