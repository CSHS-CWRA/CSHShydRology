#' Generates the x axis beginning on specified day of year
#'
#' @description Generates an axis for day of year or day of water year; used by 
#' \code{ch_regime_plot}. Obtaining the day of water year needs to be done separately.
#'  
#' @param wyear Month of beginning of water year, \code{wyear = 1} (the default) for 
#' calendar year, \code{wyear = 10} to start October 1.
#' 
#' @author Paul Whitfield 
#' @seealso \code{\link{ch_regime_plot}}
#' @return Plots a water year axis on a standard R plot
#' @export

#' @examples 
#' a <- seq(1, 365)
#' b <- runif(365)
#' plot(a, b,  type = "p", xlab = "", xaxt = "n")
#' ch_axis_doy(wyear = 10) # starts in October

ch_axis_doy <- function(wyear = 1) {
  
  cday <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366, 397, 425, 456, 486, 517, 547, 578, 609, 639, 670)
  ctxt <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
  
  wday <- cday[1:13]
  wtxt <- ctxt[1:13]
  
  if (wyear == 1) {        # starts in January
    axis(side = 1, at = wday , labels = wtxt, line = 0, tck = -0.025, xlab = "")
  } else {
    wday <- cday[wyear:(wyear + 13)] 
    offset <- (cday[wyear] - 1)
    wday[1:13] <- wday[1:13] - offset
    
    wtxt <- ctxt[wyear:(wyear + 13)]
    
    axis(side = 1, at = wday , labels = wtxt, line = 0, tck = -0.025, xlab = "")
  }
}