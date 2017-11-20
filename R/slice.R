#' Converts data into bins based upon a chosen step size
#'
#' @param doy  - an array of the day of calendar year for the dataset
#' @param step - width of bin in days
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @return 
#' {An array of bin numbers that can be used as a factor for each day in the dataset
#' prints a message indicating the handling of partial bins 
#' }
#' 
#' @export 
#'
#' 
#' @examples
#' \dontrun{dice <- slice(doy,5)}

slice <- function(doy, step)  #  use day of year 1-365 and slice into periods based upon step 5 11 or ....
{
  
  limit <- floor(366/step)
  period <- floor((doy+step-1)/step)
  
  extra <- 366 -limit*step
  
  for(k in 1:length(doy)) {
    if(period[k]>limit) period[k] <-limit
  }
  
  llevels <-as.character(c(1:limit))
  period <-factor(period, levels=llevels)
  
  print(paste("Bins =",limit," The number of extra points in last bin is up to ",extra, " per year"))
  return (period)
}