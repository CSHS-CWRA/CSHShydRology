#' Extracts peak flows over a threshold
#'
#' @description 
#' This function is development code being shared as is. It is expected that the user will be interested in the
#' data frame returned for POT analysis and for plotting (i.e. ch_booth_plot).
#'
#' This function retrieves peaks greater than or equal to the prescribed threshold.  It returns a data frame of peak characteristics
#' suitable for subsequent analysis.
#'
#' The portion under development is returns a list of the flows during an event with the values of the four
#' preceding days and three subsequent days. If the peak is a single point the fragment is nine points long; if the events is longer 
#' the fragment contains all days above the threshold and eight additional days. 
#'
#' @param dataframe a data frame of streamflow data containing columns named \option{Date} and \option{Flow}
#' @param threshold a value for the threshold. Values above the threshold are tested for peaks.
#'
#' @return Returns a list containing:
#' 	\item{POTevents}{a dataframe contining details of the events}
#' 	\item{events}{a vector with the value 0 when the flow is below the threshold and 1 when above.}
#' 	\item{event_num}{a vector with the value 0 when the flow is below a threshold or the index of the events when the threshold was exceeded. i.e. 1,2,3, etc}
#' 	\item{st_date}{start date of events}
#' 	\item{case}{a list of the daily flows in each individual event (see details for more information)}
#' 	
#' 	The \code{POTevents} data frame contains five columns: 
#' 	\item{st_date}{starting date of event}
#' 	\item{max_date}{date of maximum in the event}
#' 	\item{max}{maximum discharge during event}
#' 	\item{volume}{flow volume during the event}
#' 	\item{duration}{length of the event in days}

#' 	The \code{case} list contains the flows during an event and also for four preceding and subsequent days. Each event will have
#' 	 a length between nine to n days in length. Note: in rare cases where the event is in progress when data becomes available the 
#' 	 event might be shorter than nine days long.
#'
#' @author Paul Whitfield
#' 
#' @references
#' Burn, D.H., Whitfield, P.H., Sharif, M., 2016. Identification of changes in floods and flood regimes
#' in Canada using a peaks over threshold approach. Hydrological Processes, 39: 3303-3314. DOI:10.1002/hyp.10861
#'
#' Whitfield, P.H., and J.W. Pomeroy. 2016. Changes to flood peaks of a mountain river: implications
#' for analysis of the 2013 flood in the Upper Bow River, Canada. Hydrological Processes 30:4657-73. doi:
#' 10.1002/hyp.10957.
#' 
#' @export
#' @seealso \code{\link{ch_booth_plot}}
#' @examples
#' CAN05AA008 <- CAN05AA008
#' threshold <- 0.5*max(CAN05AA008$Flow)  # arbitrary threshold
#' my_peaks <- ch_get_peaks(CAN05AA008, threshold)
#' str(my_peaks)


ch_get_peaks <- function(dataframe, threshold) {
  
  maxflow <- max(dataframe$Flow)
  
  if (maxflow < threshold) {
    message(paste("Threshold of", threshold, "
                is greater than maximum observed flow", maxflow))
    return()
  }  
  data <- dataframe$Flow
  Date <- dataframe$Date
  
  event <-array(0, dim=length(data))
  event_num <-array(0, dim=length(data))
  flow <-array(dim=7)
  st_date <-array(NA,dim=3)
  max_date <-array(NA,dim=3)
  class(st_date) <-"Date"
  class(max_date) <-"Date"
  
  case <-list(dim=3)
  
  for (i in 1:length(data)) {
    
    if (is.na(data[i])) next
    if (data[i] > threshold) event[i] <-1 #####flag all dates above the threshold as a '1' as opposed to a zero
  }
  
  
  ##############################################  Make data frame of events and attributes
  #
  # event is an array that has 0 and 1 whenever there is a switch from zero to 1 a new event starts
  # 			whenever the event ends the switch is 1 to 0
  # 	track maximum
  #       sum volumes above thresholds
  
  
  max	  <- array(NA,dim=1)
  volume 	  <- array(NA,dim=1)
  duration  <- array(NA,dim=1)
  
  index = 1 			###  set index to increment each time a new event is detected
  
  flag = 0
  
  for (k in 1:length(data)){
    
    if (event[k] == 1 && flag==0) 					###  New Event
    {	st_date[index] <- Date[k]
    
    max[index]=data[k] 
    max_date[index] <-Date[k]
    
    volume[index] <-  0.0  
    duration[index] <-0  
    flag <- 1
    }
    
    if (event[k] ==1 && flag ==1)					###  Continuing Event
      
    {	if (data[k] > max[index]) 
    {max[index] <- data[k]
    max_date[index] <-Date[k]
    
    }
      volume[index] <- volume[index] + data[k]
      duration[index] <- duration[index] +1
    }
    
    if (event[k] ==0 && flag ==1)					###  Event has ended
    {	
      index <- index+1
      flag <- 0
    }
  }
  
  st_date <- as.Date(st_date, format="%Y-%m-%d")
  
  volume <- volume *24*60*60 *1e-9   ################### convert volumes to km#
  
  max_date <- as.Date(max_date, format="%Y-%m-%d")
  

  POT_events <- data.frame(st_date, max_date, max, volume, duration)
  

  
  #########  individual events
  
  ##############################################  Make list of individual events 
  #
  # There are two problem conditions.  The first is when the first event starts before there are four preceding events
  # and the second is when the record ends during an event, or there are not four days following the nd of an event and the end of 
  # the record.  In these two cases the event is padded with NA in places where no data existed.  These get removed later.
  #
  
  flag=0
  index=1
  flow <-array(dim=9)
  
  for (k in 1:length(data)){
    
    if (event[k] == 1 && flag==0)                                   ###  New Event
    {                     
      
      st_date[index] <- as.character(Date[k])
      
      ##### start the event with the immediately preceding 4 days
      ##### if k is less than 5 then there is not data from the preceding all of the preceding 4 days set any to NA
      
      if (k == 1) {flow[1: 4] <- NA}
      if (k == 2) {flow[1: 3]<- NA; flow[4]<- data[k-1]} 
      if (k == 3) {flow[1: 2]<- NA; flow[3] <-data[k-2] ;flow[4]<- data[k-1]} 
      if (k == 4) {flow[1]  <- NA; flow[2] <-data[k-3]; flow[3] <-data[k-2] ;flow[4]<- data[k-1]} 
      if (k <= 4) {event_num[1:4] <-index}
      
      ##### if the event is not immediately at the start of the record set to the values of the four days that precede
      if (k >5) {	
        event_num[k-1] <- index	;flow[4] <-data[k-1] 
        event_num[k-2] <- index; flow[3] <-data[k-2]
        event_num[k-3] <- index; flow[2] <-data[k-3]
        event_num[k-3] <- index; flow[1] <-data[k-4]
      }
      
      ii <-k	
      ii <-ii-5
      
      event[k-1] <-1
      event_num[k] <- index
      flag <- 1
    }
    
    if (event[k] ==1 && flag ==1)             ###  Continuing Event
    {       event_num[k] <- index
    flow[k-ii] <-data[k]
    
    }
    
    if (event[k] ==0 && flag ==1)             ###  Event has ended [first susequent day] but only if not past record end
    {      
      event_num[k]   <-index; flow[k-ii]  <-data[k]   ## add another subsequent 3 days as end of event but only if not past end of record
      
      
      if((k-length(data))>=1)	event_num[k+1] <-index; flow[k+1-ii] <-data[k+1]
      if((k-length(data))>=2)	event_num[k+2] <-index; flow[k+2-ii] <-data[k+2]
      if((k-length(data))>=3)	event_num[k+3] <-index; flow[k+3-ii] <-data[k+3]
      
      
      #############################################   if there are missing values remove them from the event
      case[index] <-list(flow[!is.na(flow)])
      
      rm(flow)   ###clear the event aray for the next case
      flow <-array(dim=9)
      
      event[k-1] <-1
      index <- index+1
      flag <- 0
    }
    
    
    
  }
  
  # event_num is an array that has 0 and an index of the sequence of events from 1 to n events
  #                       whenever the event ends the switch is  to 0
  
  
  
  
  #
  
  ncases <- length(POT_events$st_date)
  events <- list(POT_events,ncases, case)
  
  names(events) <- c("POTevents","ncases","case")
  
  
  return(events)
}






