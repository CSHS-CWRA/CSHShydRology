#' Days of year and water year
#' 
#' Converts a date array into a data frame with years, wateryears, and days of year and of water year.
#'
#' @description Converts an array of dates into a dataframe with date, year, month, doy,
#' wyear, dowy. 
#' 
#' The day of water year is computed from the first of the specified water year month.
#'
#' @param Date an array of \R dates, as produced by \code{as.Date()}
#' @param water_yr the month starting the water year, default is 10 (October). If
#' a value of \code{1} is specified, the \code{10} will be used.
#' 
#' @author Paul Whitfield, Kevin Shook
#' @return Returns a dataframe with date information:
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

ch_doys <- function(Date, water_yr = 10) # Date needs to be as.Date
{
  
  dm <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  month <- as.numeric( format(Date, "%m"))
  year  <- as.numeric( format(Date, "%Y"))
  day   <- as.numeric( format(Date, "%d"))
  doy   <- as.numeric( format(Date, "%j"))
  

  if (water_yr == 1)  
    water_yr <- 10  ## use 10 for water year if calendar year selected 
  

  year1 <- year - 1
  
  dwy <- array(NA, dim = length(Date))
  wyear <- year1
  
  wyear[month >= water_yr] <- wyear[month >= water_yr] + 1
  dwy[month >= water_yr] <- as.numeric(ISOdate(year[month >= water_yr], 
                                                 month[month >= water_yr], 
                                                 day[month >= water_yr]) - 
                                           ISOdate(wyear[month >= water_yr],
                                                   water_yr - 1, 
                                                   dm[water_yr - 1]))
   
  dwy[month < water_yr] <- as.numeric(ISOdate(year[month < water_yr], 
                                              month[month < water_yr], 
                                              day[month < water_yr]) - 
                                        ISOdate(wyear[month < water_yr], 
                                                water_yr - 1, dm[water_yr - 1]))
  

  
  dowy <- data.frame(Date, year, month, day, doy, wyear, dwy)
  
  return(dowy)
}
