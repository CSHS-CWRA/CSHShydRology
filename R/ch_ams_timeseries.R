#' Produce a Time Series Plot
#'
#' Uses ggplot2 to produce a time series plot of values (e.g., flow) 
#' over time, where time is binned by year. Subsequent years are connected with
#' a line, which is broken when data are missing.
#'
#' @param date Vector of dates or years, or name of data column containing the 
#' dates (unquoted).
#' @param flow Vector of flows, or name of data column containing the flows
#' (unquoted).
#' @param data Optional; data frame containing the specified data. If
#' specified, this data frame will be searched first when looking for data
#' vectors.
#' @returns A ggplot2 object. This means that you are able to add more layers
#' downstream.
#' @examples
#' library(lubridate)
#' set.seed(42)
#' dates <- ymd(paste(
#'   c(1991:1993, 1995, 1997:2012), "-",
#'   sample(1:12, size = 20, replace = TRUE), "-",
#'   sample(1:28, size = 20, replace = TRUE)
#' ))
#' y <- stats::rexp(20)
#' y[18] <- NA
#' ch_ams_timeseries(dates, y)
#' df <- data.frame(year = year(dates), flow = y)
#' ch_ams_timeseries(year, 35.31467 * flow, data = df) +
#'   ylab("Peak Instantaneous Flow (cfs)") +
#'   theme_bw()
#' @export
ch_ams_timeseries <- function(date, flow, data = NULL) {
  in_date <- rlang::enquo(date)
  in_flow <- rlang::enquo(flow)
  name_flow <- names(rlang::quos_auto_name(list(in_flow)))
  date <- rlang::eval_tidy(in_date, data = data)
  flow <- rlang::eval_tidy(in_flow, data = data)
  if (lubridate::is.Date(date) || lubridate::is.POSIXt(date)) {
    year <- lubridate::year(date)
  } else {
    year <- date
  }
  xy <- vctrs::vec_recycle_common(year, flow) # Enforce strict recycling.
  year <- xy[[1]]
  flow <- xy[[2]]
  complete_years <- seq(min(year, na.rm = TRUE), max(year, na.rm = TRUE))
  complete_df <- data.frame(year = complete_years)
  original_df <- data.frame(year = year, flow = flow)
  df <- dplyr::left_join(complete_df, original_df, by = "year")
  if (nrow(df) == 0) return(ggplot2::ggplot())
  ggplot2::ggplot(df, ggplot2::aes(year, flow)) +
    ggplot2::geom_point() +
    ggplot2::geom_line()
}
