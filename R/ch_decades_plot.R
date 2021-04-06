#'   Plots output from ch_binned_MannWhitney on x/y
#'   
#' @description   creates a simple plot comparing two decades from the output of ch_binned_MannWhitney
#'   
#' @author Paul Whitfield 
#' 
#' @param mplot- output from the function ch_binned_MannWitney
#
#' @return a list containing
#' @examples 
#' # mplot from ch_binned_MannWhitney
#' ch_decades_plot(mplot)   
#'   
#'

ch_decades_plot <- function (mplot)
  {
  
  mch <- c(24, NA, 25)
  disch <- expression( paste( "Median Period Discharge m"^{3}, "/sec"))
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