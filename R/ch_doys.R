#' Days of year and water year.
#' 
#' Converts a date array into a data frame with years, wateryears, and days of year and of water year.
#'
#' @description Converts an array of dates into a dataframe with date, Year, month, doy,
#' wyear, dowy. 
#' Calculates day of year and day of water year since 10.01 of present water year,
#'  using base::isodate.
#'
#' @param Date an array as.Date
#' @param water_yr the month starting the water year, default is 10 (October).
#' 
#' @author Paul Whitfield 
#' @return Returns a dataframe with different date information:
#' \item{Date}{in Date format}
#' \item{year}{numeric calendar year}
#' \item{month}{number calendar month}
#' \item{doy}{numeric day of year}
#' \item{wyear}{numeric water year starting on day 1 of selected month}
#' \item{dwy}{numeric day of water year}
#' 
#'  
#'  
#' @export 
#' 
#' @examples
#' dd <- seq.Date(as.Date("2010-01-01"), as.Date("2018-01-01"),by = 1)
#' output <- ch_doys(dd, water_yr=10)
#' head(output)
#' 

ch_doys <-function (Date, water_yr = 10) # Date needs to be as.Date
{
  
  dm <- c(  31, 28, 31, 30, 31, 30, 31, 31, 30, 31 ,30, 31)
  dm <- dm[water_yr]
  
  month <- as.numeric( format(Date, "%m"))
  year  <- as.numeric( format(Date, "%Y"))
  day   <- as.numeric( format(Date, "%d"))
  
  
  
  doy <- array(NA, dim = length(Date))
  
  for (k in 1:length(Date)) {
    
    doy[k] <- as.numeric(ISOdate(year[k], month[k], day[k]) - ISOdate(year[k] - 1, 12, 31))
  }
  
  ################# for water years starting in months 2 to 12
  
  if (water_yr == 1)  water_yr <- 10  ## use 10 for water year if calendar  year selected 
  
  dm <- c(  31, 28, 31, 30, 31, 30, 31, 31, 30, 31 ,30, 31)
  
  
  year1 <- year - 1
  
  dwy <- array(NA, dim = length(Date))
  wyear <- year1
  
  
  
  for (k in 1:length(Date)) {
    if (month[k] >= water_yr) 	
    {
      wyear[k] <- wyear[k] + 1
      dwy[k] <- as.numeric(ISOdate(year[k], month[k], day[k]) - ISOdate(wyear[k], water_yr - 1, dm[water_yr - 1]))
      
    }
    else {
      
      dwy[k] <- as.numeric(ISOdate(year[k], month[k], day[k]) - ISOdate(wyear[k], water_yr - 1, dm[water_yr - 1]))
      
    }
    
  }
  dowy <- data.frame(Date, year, month, day, doy, wyear, dwy)
  
  return(dowy)
}
