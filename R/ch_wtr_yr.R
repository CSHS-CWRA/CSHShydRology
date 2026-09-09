#' @title Designation of the water year
#' @description Display water year
#' @export
#'
#' @param dates A vector of dates with actual year
#' @param start_month Month in which the year starts (defaults to October)
#'
#' @return A numeric vector of water years, labelled by the calendar year in
#' which the water year \emph{ends}. With the default \code{start_month = 10},
#' October 2011 to September 2012 is water year 2012. Note that the \code{wyear}
#' column returned by \code{\link{ch_doys}} uses the opposite convention and would
#' label the same period 2011.
#' @seealso \code{\link{ch_doys}}
#'
#' @examples
#' date <- seq(as.Date("1910/1/1"), as.Date("1912/1/1"), "days")
#' wtr_yr_date <- ch_wtr_yr(dates=date, start_month=10)
#' df <- data.frame(wtr_yr_date, date)
#' @source http://stackoverflow.com/questions/27626533/r-create-function-to-add-water-year-column

ch_wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}
