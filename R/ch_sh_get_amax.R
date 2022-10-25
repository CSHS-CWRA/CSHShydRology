#'  Extracts annual maximum values from ECDE dataframe. 
#'
#' @description 
#' Extracts annual maximum values, the date of occurrence, the day of year, and the completeness 
#' from ECDE dataframe. Uses functions from timeDate ( \code{as.timeDate}, \code{dayOfYear}).
#'
#' @param df A dataframe of daily streamflow data from ECDE
#' 
#' @return Returns a dataframe with the following variables  
#'  \item{year}{}
#'  \item{annual maximum}{}
#'  \item{date of annual maximum}{}
#'  \item{day of year of annual maximum}{}
#'  \item{days}{number of days with observations}
#'
#' @export
#' 
#' @author Paul Whitfield
#' @seealso  \code{\link{ch_read_ECDE_flows}}  \code{\link{ch_circ_mean_reg}}   
#' @examples
#' data(CAN05AA008)
#' amax <- ch_sh_get_amax(CAN05AA008)
#' str(amax)


ch_sh_get_amax <- function(df) {
  
  data <- df$Flow
  Date <- df$Date
  year <- format(Date, "%Y")
  Year <- as.numeric(unique(year))
  maxdate <- array(NA, dim = length(Year))
  doy <- array(NA, dim = length(Year))
  days <- array(NA, dim = length(Year))
  class(maxdate) <- "Date"
  year <- as.factor(year)
  
  amax <- as.numeric(tapply(data,year,max))
  
  dataframe <- data.frame(df,year)
  
  for (k in 1:length(Year)) {
    ndata <- dataframe[dataframe$year == Year[k],]
    days[k] <- length(ndata$Flow)
    
    ndata <- ndata[ndata$Flow == amax[k],]
    maxdate[k] <- ndata[1, 3]
    maxdate_a <- timeDate::as.timeDate(maxdate[k])
    doy[k] <- timeDate::dayOfYear(maxdate_a)
  }
  
  result <- data.frame(Year,amax, maxdate, doy, days)
  
  return(result)
  
}