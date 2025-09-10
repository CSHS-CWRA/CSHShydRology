#' Hydrographs for WSC stations using \pkg{ggplot2}
#' 
#' @description
#' Acquires and plots values for WSC streamflows, using \pkg{ggplot2}. The existing 
#' functions \code{ch_qa_hydrograph} and
#' \code{ch_model_hydrograph} use basic \R plotting, require other functions to 
#' assemble the values, and only plot
#' values for a single station. This function is
#' able to plot hydrographs for more than one station, which may be useful, particularly 
#' when comparing responses for several streams in the same region.
#' @param WSC_stations Required. A vector of WSC station numbers.
#' @param daily Optional. If TRUE, mean daily streamflows are plotted as stair-steps.
#' @param instantaneous Optional. If TRUE, annual instantaneous peak flows are plotted as points.
#' @param facets Optional. If TRUE, the plot is faceted by station number.
#' @param common_dates Optional. If TRUE, a common date range is used for all time series.
#' @param start_date Optional. If specified (format = yyyy-mm-dd), only values on or following the date are plotted.
#' @param end_date Optional. If specified (format = yyyy-mm-dd), only values on or before the date are plotted.
#' @param hydat_path Optional. Path to the HYDAT database. Usually omitted unless you want to use a specific database file.
#' @param ... Other parameters for the \pkg{ggplot} facets, if specified.
#' @author Kevin Shook
#' @seealso  \code{\link{ch_qa_hydrograph}}  \code{\link{ch_model_hydrograph}} 
#' @returns Returns a \code{ggplot2} object of the hydrographs.
#' @export
#' @import ggplot2 dplyr tidyhydat
#'
#' @examples {
#' # plot a single station
#' stations <- c("05HH003")
#' p <- ch_gg_hydrographs(stations, daily = TRUE, instantaneous = TRUE)
#' 
#' # plot a group of stations in a region, as all appear to be responding to the same event
#' stations <-  c("05CC001", "05CC011", "05CD006", "05CD007", "05CE002", "05CE006", 
#' "05CE010", "05CE012", "05CE018", "05CE020", "05CG004", "05CG006")
#' 
#' p <- ch_gg_hydrographs(stations, daily = TRUE, instantaneous = FALSE, 
#' common_dates = FALSE, start_date = "2011-06-01", end_date = "2011-06-30", 
#' facets = TRUE, scales = "free_y", ncol= 3)
#' }

ch_gg_hydrographs <- function(WSC_stations, 
                               daily = TRUE, 
                               instantaneous = FALSE, 
                               facets = TRUE, 
                               common_dates = FALSE,
                               start_date = NULL, 
                               end_date = NULL,
                               hydat_path = NULL, 
                              ...) {
  # set up plot values
  Datetime <- NULL 
  Value <- NULL
  start_year <- NULL
  end_year <- NULL
  STATION_NUMBER <- NULL
  
  # check parameter values
  if (!daily & !instantaneous)
    stop("No plots selected")
  
  if (is.null(WSC_stations) | length(WSC_stations) == 0 )
    stop("No stations selected")
  
  if (!is.null(start_date)) {
    start_date_val <- as.Date(start_date, format = "%Y-%m-%d")
    start_year <- as.numeric(format(start_date_val, format = "%Y"))
  }
  
  if (!is.null(end_date)) {
    end_date_val <- as.Date(end_date, format = "%Y-%m-%d")
    end_year <- as.numeric(format(end_date_val, format = "%Y"))
  }
    
  # get WSC data for plotting and find min and max dates
  if (daily) {
    wsc_daily <- hy_daily_flows(WSC_stations, 
                                start_date = start_date, 
                                end_date = end_date)
    
    wsc_daily$Datetime <- as.POSIXct(wsc_daily$Date, format = "%Y-%m-%d")
    daily_min_max_dates <- wsc_daily %>% group_by(STATION_NUMBER) %>% 
      summarise(min_Datetime = min(Datetime), max_Datetime = max(Datetime))
    
    common_daily_min_Datetime <- max(daily_min_max_dates$min_Datetime)
    common_daily_max_Datetime <- min(daily_min_max_dates$max_Datetime)
  }
  
  if (instantaneous) {
    wsc_inst <- hy_annual_instant_peaks(WSC_stations,
                                        start_year = start_year, 
                                        end_year = end_year)
    
    wsc_inst <- wsc_inst[wsc_inst$Parameter == "Flow" & wsc_inst$PEAK_CODE == "MAX",]
    
    # remove wsc_inst values with missing datetimes
    wsc_inst <- wsc_inst[!is.na(wsc_inst$Datetime),]
    
    inst_min_max_dates <- wsc_inst %>% group_by(STATION_NUMBER) %>% 
      summarise(min_Datetime = min(Datetime), max_Datetime = max(Datetime))
    common_inst_min_Datetime <- max(inst_min_max_dates$min_Datetime)
    common_inst_max_Datetime <- min(inst_min_max_dates$max_Datetime)
  }
  
  if (common_dates) {
    if (daily & !instantaneous) {
      common_min_Datetime <- common_daily_min_Datetime
      common_max_Datetime <- common_daily_max_Datetime
      wsc_daily <- wsc_daily[wsc_daily$Datetime >= common_min_Datetime,]
      wsc_daily <- wsc_daily[wsc_daily$Datetime <= common_max_Datetime,]
    } else if (!daily & instantaneous) {
      common_min_Datetime <- common_inst_min_Datetime
      common_max_Datetime <- common_inst_max_Datetime
      
      wsc_inst <- wsc_inst[wsc_inst$Datetime >= common_min_Datetime,]
      wsc_inst <- wsc_inst[wsc_inst$Datetime <= common_max_Datetime,]
      
    } else {
      common_min_Datetime <- common_daily_min_Datetime
      common_max_Datetime <- common_daily_max_Datetime
      
      wsc_daily <- wsc_daily[wsc_daily$Datetime >= common_min_Datetime,]
      wsc_daily <- wsc_daily[wsc_daily$Datetime <= common_max_Datetime,]
      
      wsc_inst <- wsc_inst[wsc_inst$Datetime >= common_min_Datetime,]
      wsc_inst <- wsc_inst[wsc_inst$Datetime <= common_max_Datetime,]
    }
  }
   # plot
  if (daily & !instantaneous) {
    if (!facets)
      p <- ggplot(wsc_daily, aes(Datetime, Value, colour = STATION_NUMBER)) +
        geom_step(direction = "hv")
    else
      p <- ggplot(wsc_daily, aes(Datetime, Value)) +
        geom_step(direction = "hv") +
        facet_wrap(~STATION_NUMBER, ...)
      
  } else if (!daily & instantaneous) {
    if (!facets)
      p <- ggplot(wsc_inst, aes(Datetime, Value, colour = STATION_NUMBER)) +
        geom_point()
    else
      p <- ggplot(wsc_inst, aes(Datetime, Value)) +
        geom_point() +
        facet_wrap(~STATION_NUMBER, ...)
    
  } else {
    if (!facets){
      p <- ggplot(wsc_daily, aes(Datetime, Value, colour = STATION_NUMBER)) +
        geom_step(direction = "hv") +
        geom_point(data = wsc_inst, aes(Datetime, Value), ...)
    }
    else
      p <- ggplot(wsc_daily, aes(Datetime, Value)) +
        geom_step(direction = "hv") +
        geom_point(data = wsc_inst, aes(Datetime, Value)) +
        facet_wrap(~STATION_NUMBER, ...)
  }
  
  # add labels
  p <- p + xlab("") + ylab(expression(paste("Discharge (m", ""^{ 3 }, "/s)", sep = "")))
  
  return(p)
}