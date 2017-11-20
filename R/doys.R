#' Multiple function Day of Year, Water Year Function
#'
#' @param {Date} {an array as.Date}
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @return a dataframe with different format dates
#' @return \code{Date}  in Date format
#' @return \code{year}  numeric calendar year
#' @return \code{doy}   numeric day of year
#' @return \code{wyear} numeric day of water year starting on 1 October
#' @return \code{dwy}   numeric day of water year
#' 
#'  Conversion of a data array to a dataframe of different forms date,doy, dowy, Year
#'  Calculates day of year and day of water year since 10.01 of present water year.
#'  uses base::isodate
#' @export 
#' 
#' @examples
#' \dontrun{my_dates <- doys(mdata$Date)}
#' 
#' 
#' 

doys <-function (Date) # Date needs to be as.Date
{
  
  mon <- as.numeric( format(Date, "%m"))
  year  <- as.numeric( format(Date, "%Y"))
  day   <- as.numeric( format(Date, "%d"))
  
  
  doy <-array(NA,dim=length(Date))
  for (k in 1:length(Date)) {
    doy[k] <- as.numeric(ISOdate(year[k],mon[k],day[k]) - ISOdate(year[k]-1,12,31))
  }
  
  year1 <- year-1
  
  dwy <-array(NA,dim=length(Date))
  wyear <- array(NA,dim=length(Date))
  
  for (k in 1:length(Date)) {
    if (mon[k] >=10) 	
    {
      dwy[k] <- as.numeric(ISOdate(year[k],mon[k],day[k]) - ISOdate(year[k],9,30))
      wyear[k] <-year[k]+1
    }
    else {
      dwy[k] <- as.numeric(ISOdate(year[k],mon[k],day[k]) - ISOdate(year1[k],9,30))
      wyear[k] <-year[k]
    }
  }
  dowy <- data.frame(Date, year, doy, wyear, dwy)
  
  return (dowy)
}
