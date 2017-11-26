#' @title Designation of the water year
#' @description Display water year
#' @export
#'
#' @param dates A vector of dates with actual year
#' @param start_month Month in which the year starts (defaults to October)
#'
#' @return Year starting in start_month
#'
#' @examples
#' date <- seq(as.Date("1910/1/1"), as.Date("1912/1/1"), "days")
#' wtr_yr_date <- wtr_yr(dates=date, start_month=10)
#' data.frame(wtr_yr_date,date)
#' @source http://stackoverflow.com/questions/27626533/r-create-function-to-add-water-year-column

wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}
