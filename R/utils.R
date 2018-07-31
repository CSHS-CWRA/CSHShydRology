#'@title  Generates the x axis for day of year
#'
#'@description  Used by \code{regime_plot}. This code deals only with the axis adjustments. Day of water year needs to be done separately
#' @param wyear Month to begin water year. Use \code{wyear = 1} for calendar year, \code{wyear = 10} for October 1.
#' @author Paul Whitfield
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
    graphics::axis(side = 1, at = wday, labels = wtxt, line = 0, tck = -0.025, xlab = "", xlab = "")
    return()
  }

  if (wyear != 1) {
    wday <- cday[wyear:(wyear + 13)]
    offset <- (cday[wyear] - 1)
    wday[1:13] <- wday[1:13] - offset

    wtxt <- ctxt[wyear:(wyear + 13)]

    graphics::axis(side = 1, at = wday, labels = wtxt, line = 0, tck = -0.025, xlab = "", xlab = "")
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
#'
#' @seealso binned_MannWhitney, raster_trend
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
