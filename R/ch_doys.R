#' Days of year and water year
#' 
#' Converts a date array and produces a data frame with years, wateryears, and days of year and of water year.
#'
#' @description Converts an array of dates into a dataframe with date, Year, month, doy,
#' wyear, dowy. Calculates day of year and day of water year since 10.01 of present water year,
#'  using base::isodate.
#'
#' @param {Date} {an array as.Date}
#' @param {w_yr} the month starting the water year, default is 10 (October).
#' 
#' @author Paul Whitfield 
#' @return a dataframe with different date information
#' @return \code{Date}  in Date format
#' @return \code{year}  numeric calendar year
#' @return \code{month} number calendar month
#' @return \code{doy}   numeric day of year
#' @return \code{wyear} numeric water year starting on day 1 of selected month
#' @return \code{dwy}   numeric day of water year
#' 

#'  
#'  
#' @export 
#' 
#' @examples
#' dd <- seq.Date(as.Date("2010-01-01"), as.Date("2018-01-01"),by = 1)
#' output <- doys(dd, w_yr=10)
#' head(output)
#' 

ch_doys <-function (Date, w_yr = 10) # Date needs to be as.Date
{
  
  dm <- c(  31, 28, 31, 30, 31, 30, 31, 31, 30, 31 ,30, 31)
  dm <- dm[w_yr]
  
  month <- as.numeric( format(Date, "%m"))
  year  <- as.numeric( format(Date, "%Y"))
  day   <- as.numeric( format(Date, "%d"))
  
  
  
  doy <- array(NA, dim = length(Date))
  
  for (k in 1:length(Date)) {
    
    doy[k] <- as.numeric(ISOdate(year[k], month[k], day[k]) - ISOdate(year[k] - 1, 12, 31))
  }
  
  ################# for water years starting in months 2 to 12
  
  if (w_yr == 1)  w_yr <- 10  ## use 10 for water year if calendar  year selected 
  
  dm <- c(  31, 28, 31, 30, 31, 30, 31, 31, 30, 31 ,30, 31)
  
  
  year1 <- year - 1
  
  dwy <- array(NA, dim = length(Date))
  wyear <- year1
  
  
  
  for (k in 1:length(Date)) {
    if (month[k] >= w_yr) 	
    {
      wyear[k] <- wyear[k] + 1
      dwy[k] <- as.numeric(ISOdate(year[k], month[k], day[k]) - ISOdate(wyear[k], w_yr - 1, dm[w_yr - 1]))
      
    }
    else {
      
      dwy[k] <- as.numeric(ISOdate(year[k], month[k], day[k]) - ISOdate(wyear[k], w_yr - 1, dm[w_yr - 1]))
      
    }
    
  }
  dowy <- data.frame(Date, year, month, day, doy, wyear, dwy)
  
  return(dowy)
}
