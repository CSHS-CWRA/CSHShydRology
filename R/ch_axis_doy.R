#' Generates the x-axis for day of year or water year
#'  
#' 
#'  
#' @description Generates the x-axis for day of year or day of water year to be 
#'  used by \code{ch_regime_plot}. Day of water year needs to be generated separately.
#' 
#'  
#' @param wyear wyear = 1 for calendar year, = 10 for October 1.
#' @return No value is returned; the axis of the axis of the \R graphic created by \code{ch_regime_plot} is modified. 
#' @author Paul Whitfield 
#' @seealso \code{\link{ch_regime_plot}}
#' @export

#' @examples 
#' \dontrun{
#' ch_axis_doy(wyear = 1)  # starts in January
#' ch_axis_doy(wyear = 10) # starts in October
#' }

ch_axis_doy <- function (wyear = 1) 
  {
  
  
  cday <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366, 397, 425, 456, 486, 517, 547, 578, 609, 639, 670)
  ctxt <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
  
  wday <- cday[1: 13]
  wtxt <- ctxt[1: 13]
  
  if (wyear == 1){        # starts in January
    axis(side = 1, at = wday , labels = wtxt, line = 0, tck = -0.025, xlab="")
    return()
  }
  
  if (wyear != 1){
    wday <- cday[wyear:(wyear + 13)] 
    offset <-(cday[wyear] - 1)
    wday[1:13] <- wday[1:13] - offset
    
    wtxt <- ctxt[wyear:(wyear + 13)]
    
    axis(side = 1, at = wday , labels = wtxt, line = 0, tck = -0.025, xlab="")
    return()
  }
}