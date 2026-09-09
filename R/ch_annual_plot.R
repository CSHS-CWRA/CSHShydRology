#' Plot a series of annual values
#'
#' Uses ggplot2 to produce a time series plot of values (e.g., annual maximum
#' flow) over time, where time is binned by year.
#'
#' Every year between the first and last is included, so a year with no data
#' carries an \code{NA} and the connecting line is broken there rather than
#' drawn across the gap. This is deliberate: a continuous line across a period
#' of missing record implies a continuity that the data do not support.
#'
#' The function expects at most one value per year. If a year appears more than
#' once, every value is kept and plotted, so the line doubles back on itself
#' within that year. This is not checked for, and no warning is given.
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
#' library(ggplot2)
#'
#' # Annual maximum series for a Water Survey of Canada gauge.
#' ams <- ch_rfa_extractamax(Flow ~ Date, CAN05AA008, tol = 350)
#' ch_annual_plot(Date, Flow, data = ams)
#'
#' # The returned object is a ggplot, so it can be added to.
#' ch_annual_plot(Date, 35.31467 * Flow, data = ams) +
#'   ylab("Annual maximum daily discharge (cfs)") +
#'   theme_bw()
#'
#' # Vectors work too, from any source.
#' ch_annual_plot(ams$Date, ams$Flow)
#' @export
ch_annual_plot <- function(date, flow, data = NULL) {
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
