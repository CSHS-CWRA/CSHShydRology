#'@title  Generates the x axis for day of year
#'
#'@description  Used by \code{ch_regime_plot}. This code deals only with the axis adjustments. Day of water year needs to be done separately
#'@param wyear Month to begin water year. Use \code{wyear = 1} for calendar year, \code{wyear = 10} for October 1.
#'@author Paul Whitfield
#'@keywords internal
#'@import graphics
#'
axis_doy <- function(wyear = 1) {
  cday <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366, 397, 425, 456, 486, 517, 547, 578, 609, 639, 670)
  ctxt <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"
  )

  wday <- cday[1:13]
  wtxt <- ctxt[1:13]

  if (wyear == 1) { # starts in January
    axis(side = 1, at = wday, labels = wtxt, line = 0, tck = -0.025, xlab = "", xlab = "")
    return()
  }

  if (wyear != 1) {
    wday <- cday[wyear:(wyear + 13)]
    offset <- (cday[wyear] - 1)
    wday[1:13] <- wday[1:13] - offset

    wtxt <- ctxt[wyear:(wyear + 13)]

    axis(side = 1, at = wday, labels = wtxt, line = 0, tck = -0.025, xlab = "", xlab = "")
    return()
  }
}

#'@title Converts doy or dwy into a factor that is used to bin data
#'
#'@description Whenever the number of bins does not divide in 365 evenly a message is printed showing the number of bins created and the number of days added to the last bin. Simply put, \code{slice} is used to convert doy into a factor which is a number of bins per year. A year can be converted into any number of bins; slice does it based upon a number of days. So when you send it am array of doy it slices that into bins of the desired width. For example, if the step is 5. They 365/5 gives 73 bins and becasue of leap years there might be one extra day added every four years to the final bin.
#'
#' To illustrate:
#' doy:
#'   1  2  3  4  5  6  7  8  9 10 11  12
#' Bin:
#'   1  1  1  1  1  2  2  2  2  2  3   3
#'
#' @param doy A vector of the day of calendar year for the dataset
#' @param step Width of bin in days
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @return Returns a vector of bin numbers that is used as a factor for each day in the dataset
#' prints a message indicating the handling of partial bins
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{ch_binned_MannWhitney}} \code{\link{ch_flow_raster_trend}}
#'
#' @examples
#' doy <- c(1:365)
#' # first 30 days are 1, 31-60 are 2 etc
#' dice <- slice(doy, 30)
#' plot(doy, dice)

slice <- function(doy, step) {
  limit <- floor(366 / step)
  period <- floor((doy + step - 1) / step)

  extra <- 366 - limit * step

  for (k in 1:length(doy)) {
    if (period[k] > limit) period[k] <- limit
  }

  llevels <- as.character(c(1:limit))
  period <- factor(period, levels = llevels)

  print(paste("Bins =", limit, " The number of extra points in last bin is up to ", extra, " per year"))
  return(period)
}

#' @title Helper function for selecting points for an axis so not all are necessary
#'
#' @description Sub-samples a vector every n places. Many times there are so many years the labels on the plot overlap. This function returns the position and label for the subset. The function can be used on any type of simple array.
#' @param years a vector of years
#' @param n sample size
#' @return a list containing:
#' \describe{
#' 	\item{position}{array of axis positions}
#' 	\item{label}{array of labels}
#' 	}
#' @export
#' @keywords internal
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @examples
#' myears <- c(1900:2045)
#' myears <- sub_set_Years(myears, 20)
#' myears
#'
#' a <- LETTERS
#' my_alpha <- sub_set_Years(a, 5)
#' my_alpha

sub_set_Years <- function(years, n) {
  pts <- c(1:length(years))
  pts <- pts[1:(length(years) / n) * n]

  years <- years[pts]

  result <- list(pts, years)
  names(result) <- c("position", "label")
  return(result)
}

#' @title Cuts a block in time from a time series
#' 
#' @description Allows the user to select a time period from a longer record. Could be used to
#' get the same period of time from several stations for comparison.
#'
#'
#' @param dataframe A time series dataframe with a \code{Date} variable
#' @param st_date Starting date as a string with the format \option{Y/m/d}
#' @param end_date Ending date as a string with the format \option{Y/m/d}
#' 
#' @return Returns a data frame with the same columns as the original data frame
#' @export
#' @keywords internal
#' @author Paul Whitfield
#' @examples 
#' subset <- cut_block(W05AA008, "2000/01/01", "2010/12/31")

cut_block <- function(dataframe, st_date, end_date) {
  
  st_date  <- as.Date(st_date, format = "%Y/%m/%d")
  if (!st_date >= min(dataframe$Date)) {
    print(paste("Starting Date",st_date, "is before records are available"))
    return()
  }
  
  if (!end_date <= max(dataframe$Date)) {
    print(paste("Ending Date",st_date, "is after records are available"))
    return()
  }
  
  end_date <- as.Date(end_date, format = "%Y/%m/%d")
  
  result1 <- dataframe[dataframe$Date >= st_date,]
  
  
  
  result <- result1[result1$Date <= end_date,]
  print(paste("between",st_date,"and", end_date, length(result[,1]), 
              "records were selected"))
  
  return(result)
}


#' Stacks EC values
#' 
#' @description Converts data frames of Environment Canada year x month or month x day data to vectors
#' @param data_values Required. Data frame of year x month or month x day values.
#' @param data_codes Required. Data frame of year x month or month x day data codes.
#'
#' @return Returns a data frame with two colums: the data values, and the data codes.
#' @export
#' @keywords internal
#'
#' @examples \dontrun{df <- unstack_EC(data_values, data_codes)}
#' 
stack_EC <- function(data_values = NULL, data_codes = NULL) {
  #check parameters
  if (is.null(data_values))  {
    stop("No specified data values")
  }
  
  if (is.null(data_codes)) {
    stop("No specified data codes")
  }
  
  # transpose data
  data_values_t <- t(data_values)
  data_codes_t <- t(data_codes)
  
  # now stack data frames to vectors
  data_values <- as.vector(data_values_t, mode = 'numeric')
  data_codes <- as.character(as.vector(data_codes_t, mode = 'character'))
  
  df <- data.frame(data_values, data_codes)
  return(df)
}

#' Days of year and water year
#' 
#' @description Converts a vector of dates into a dataframe with date, doy, dowy, year

#' @param Date A vector of R dates.
#' @param mon The month starting the water year, default is 10 (October).
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @return Returns a dataframe with differently-formatted dates
#' \describe{
#'  \item{Date}{original Date}
#'  \item{year}{numeric calendar year}
#'  \item{month}{numeric calendar month}
#'  \item{doy}{numeric day of year}
#'  \item{wyear}{numeric water year}
#'  \item{dwy}{numeric day of water year}
#'}

#'
#' @export
#' @keywords internal
#'
#' @examples
#' dd <- seq.Date(as.Date("2010-01-01"), as.Date("2018-01-01"), by = 1)
#' output <- doys(dd)
#' head(output)
#'

doys <- function(Date, mon = 10) # Date needs to be as.Date
{
  if (mon == 2) print("Currently restricted to water year starting March to October")
  
  dm <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  dm <- dm[mon - 1]
  
  month <- as.numeric(format(Date, "%m"))
  year <- as.numeric(format(Date, "%Y"))
  day <- as.numeric(format(Date, "%d"))
  
  
  doy <- array(NA, dim = length(Date))
  for (k in 1:length(Date)) {
    doy[k] <- as.numeric(ISOdate(year[k], month[k], day[k]) - ISOdate(year[k] - 1, 12, 31))
  }
  
  year1 <- year - 1
  
  dwy <- array(NA, dim = length(Date))
  wyear <- array(NA, dim = length(Date))
  
  for (k in 1:length(Date)) {
    if (month[k] >= mon) {
      dwy[k] <- as.numeric(ISOdate(year[k], month[k], day[k]) - ISOdate(year[k], mon - 1, dm))
      wyear[k] <- year[k] + 1
    }
    else {
      dwy[k] <- as.numeric(ISOdate(year[k], month[k], day[k]) - ISOdate(year1[k], mon - 1, dm))
      wyear[k] <- year[k]
    }
  }
  dowy <- data.frame(Date, year, month, doy, wyear, dwy)
  
  return(dowy)
}

#' Subset date by String

#' @description Subsets a data frame by an specified date range, provided as
#' a string by the \code{prd} argument. This function is meant to emulate the subsetting
#' capability of the \pkg{xts} package.
#'
#' @param df data frame of time series data; includes a variable called \code{Date}
#' @param prd date range as string formatted as \option{YYYY-MM-DD/YYYY-MM-DD}
#' @return \item{df}{subsetted data frame}
#' @keywords date data subset
#' @author Robert Chlumsky <rchlumsk@gmail.com>
#' @export
#' @keywords internal
#' @examples{
#' dd <- seq.Date(as.Date("2010-10-01"), as.Date("2013-09-30"), by = 1)
#' x <- rnorm(length(dd))
#' y <- abs(rnorm(length(dd)))*2
#' df <- data.frame("Date" = dd,x,y)
#' prd <- "2011-10-01/2012-09-30"
#' summary(date_subset(df,prd))}

date_subset <- function(df, prd) {
  ss <- unlist(strsplit(prd, split = "/"))
  df <- df[df$Date >= as.Date(ss[1]) & df$Date <= as.Date(ss[2]), ]
  return(df)
}
