#' Extracts a specified time period from a longer record
#' 
#' @description The function could also be used to get the same period of time from several station for comparison.
#' @param DF A daily streamflow data frame as from \code{ch_read_ECDE_flows}
#' @param st_date starting date format is \%Y/\%m/\%d
#' @param end_date ending date format is \%Y/\%m/\%d
#' 
#' @return Returns a portion of the original dataframe.
#' 
#' @export
#' @author Paul Whitfield  
#' @examples
#' data(CAN05AA008)
#' subset <- ch_cut_block(CAN05AA008,"2000/01/01", "2010/12/31")

ch_cut_block <- function (DF, st_date, end_date) 
{
  if (substr(st_date,5,5) == "/") {
    st_date  <- as.Date(st_date, format = "%Y/%m/%d")
    end_date <- as.Date(end_date, format = "%Y/%m/%d")
  }
  
  if (substr(st_date,5,5) == "-") {
    st_date  <- as.Date(st_date, format = "%Y-%m-%d")
    end_date <- as.Date(end_date, format = "%Y-%m-%d")
  } else {
    message( paste("incorrect date format", st_date, "must be like  2010/01/01"))
    return ()
  }

  if (!st_date >= min(DF$Date)){
    message(paste("Starting Date",st_date, "is before records are available"))
    return()
  }

  if (!end_date <= max(DF$Date)){
    message(paste("Ending Date",end_date, "is after records are available"))
    }

  result1 <- DF[DF$Date >= st_date, ]
  result <- result1[result1$Date <= end_date, ]
  
  message(paste("between",st_date,"and", end_date, length(result[ , 1]),
              "records were selected"))
  
  return(result)
}
