#' Extracts peak flows over a threshold
#'
#' @description Extracts peaks over a prescribed threshold (POT).
#'
#'
#' @param dataframe a data frame of streamflow data containing columns named \option{Date} and \option{Flow}
#' @param threshold a value for the threshold. Values above the threshold are tested for peaks.
#'
#' @return a list containing:
#' 	\describe{
#' 	\item{POTevents}{a dataframe contining details of the events}
#' 	\item{events}{a vector with the value 0 when the flow is below the threshold and 1 when above.}
#' 	\item{event_num}{a vector with the value 0 when the flow is below a threshold or the index of the events
#' 	when the threshold was exceeded. i.e. 1,2,3, etc}
#' 	\item{st_date}{start date of events}
#' 	\item{case}{a list of the flows in each individual event (see details for more information)}
#'}
#' 	The \code{POTevents} dataframe contains five columns: st_date (starting date), max_date (date of maximum in the event),
#' 	max (maximum discharge), volume (volume of the event), and duration (in days).
#'
#' 	The \code{case} list contains the flows during an event and also for three preceeding and subsequent days. The lists
#' 	 range from seven to n days in length.
#'
#' @references
#' Burn, D.H., Whitfield, P.H., Sharif, M., 2016. Identification of changes in floods and flood regimes
#' in Canada using a peaks over threshold approach. Hydrological Processes, 39: 3303-3314. DOI:10.1002/hyp.10861
#'
#' Whitfield, P.H., and J.W. Pomeroy. 2016. Changes to flood peaks of a mountain river: implications
#' for analysis of the 2013 flood in the Upper Bow River, Canada. Hydrological Processes 30:4657-73. doi:
#' 10.1002/hyp.10957.
#' @export
#' @seealso \code{\link{ch_booth_plot}}
#' @examples
#' threshold <- 0.9*max(W05AA008$Flow)  # arbitrary threshold
#' my_peaks <- ch_get_peaks(W05AA008, threshold)
#' str(my_peaks)

ch_get_peaks <- function(dataframe, threshold) {
  data <- dataframe$Flow
  Date <- dataframe$Date
  event <- array(0, dim = length(data))
  event_num <- array(0, dim = length(data))
  flow <- array(dim = 7)
  st_date <- array(dim = 3)
  case <- list(dim = 3)

  for (i in 1:length(data)) {
    if (is.na(data[i])) next
    if (data[i] > threshold) event[i] <- 1 ##### flag all dates above the threshold as a '1' as opposed to a zero
  }


  #############################################
  #  Make data frame of events and attributes
  #
  # event is an array that has 0 and 1 whenever there is a switch from zero to 1 a new event starts
  # 			whenever the event ends the switch is 1 to 0
  # 	track maximum
  #       sum volumes above thresholds

  st_date <- character(length = 1)
  max_date <- character(length = 1)
  max <- array(NA, dim = 1)
  volume <- array(NA, dim = 1)
  duration <- array(NA, dim = 1)

  index <- 1 ###  set index to increment each time a new event is detected

  flag <- 0

  for (k in 1:length(data)) {
    if (event[k] == 1 && flag == 0) ###  New Event
    {
      st_date[index] <- as.character(Date[k])
      max[index] <- data[k]
      max_date[index] <- as.character(Date[k])
      volume[index] <- 0.0
      duration[index] <- 0
      flag <- 1
    }

    if (event[k] == 1 && flag == 1) ###  Continuing Event
    {
      if (data[k] > max[index]) {
        max[index] <- data[k]
        max_date[index] <- as.character(Date[k])
      }
      volume[index] <- volume[index] + data[k]
      duration[index] <- duration[index] + 1
    }

    if (event[k] == 0 && flag == 1) ###  Event has ended
    {
      index <- index + 1
      flag <- 0
    }
  }

  st_date <- as.Date(st_date, origin = "1900-01-01")

  volume <- volume * 24 * 60 * 60 * 1e-9 ######################### convert volumes to km#

  max_date <- as.Date(max_date, format = "%Y-%m-%d")

  POT_events <- data.frame(st_date, max_date, max, volume, duration)

  ####################################################

  ##############################################  Make list of individual events

  flag <- 0
  index <- 1
  for (k in 1:length(data)) {
    if (event[k] == 1 && flag == 0) ###  New Event
    {
      st_date[index] <- as.character(Date[k])

      ##### start the event with the immediately preceding 3 days
      ##### if k is less than 4 then there is not data from the preceding three days set to NA

      if (k <= 3) {
        flow[1:3] <- NA
      }

      ##### else set to the values of the three days that precede
      if (k > 3) {
        event_num[k - 1] <- index
        flow[3] <- data[k - 1]
        event_num[k - 2] <- index
        flow[2] <- data[k - 2]
        event_num[k - 3] <- index
        flow[1] <- data[k - 3]
      }

      ii <- k
      ii <- ii - 4

      event[k - 1] <- 1
      event_num[k] <- index
      flag <- 1
    }

    if (event[k] == 1 && flag == 1) ###  Continuing Event
    {
      event_num[k] <- index
      flow[k - ii] <- data[k]
    }

    if (event[k] == 0 && flag == 1) ###  Event has ended but only if not past record end
    {
      event_num[k] <- index
      flow[k - ii] <- data[k] ## add next  3 days as end of event
      if ((k - length(data)) >= 1) event_num[k + 1] <- index
      flow[k + 1 - ii] <- data[k + 1]
      if ((k - length(data)) >= 2) event_num[k + 2] <- index
      flow[k + 2 - ii] <- data[k + 2]

      case[index] <- list(flow)
      rm(flow)
      flow <- array(dim = 7)

      event[k - 1] <- 1
      index <- index + 1
      flag <- 0
    }
  }

  # event_num is an array that has 0 and an index of the sequence of events from 1 to n events
  #                       whenever the event ends the switch is  to 0
  events <- list(POT_events, event, event_num, st_date, case)
  names(events) <- c("POTevents", "event", "event_num", "st_date", "case")

  return(events)
}
