#' Converts doy or dwy into a factor that is used to bin data
#'
#'@description convert doy into a factor which is a number of bins per year.
#'
#'
#' @param doy A vector of the day of calendar year for the dataset
#' @param step Width of bin in days
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @return Returns a vector of bin numbers that is used as a factor for each day 
#' in the dataset and prints a message indicating the handling of partial bins
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{ch_binned_MannWhitney}} \code{\link{ch_flow_raster_trend}}
#'
#' @examples
#' doy <- c(1:365)
#' # first 30 days are 1, 31-60 are 2 etc
#' dice <- ch_slice(doy, 30)
#' plot(doy, dice)

ch_slice <- function(doy, step) {
  limit <- floor(366 / step)
  period <- floor((doy + step - 1) / step)
  
  extra <- 366 - limit * step
  
  for (k in 1:length(doy)) {
    if (period[k] > limit) period[k] <- limit
  }
  
  llevels <- as.character(c(1:limit))
  period <- factor(period, levels = llevels)
  
  print(paste("Bins =", limit, " The number of extra points in last bin is up to ", 
              extra, " per year"))
  return(period)
}