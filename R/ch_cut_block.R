#' Extracts a specific time period from a longer record
#' 
#' @description Extracts a specific time period from a longer record.
#'  Could be used to get the same period of time from several stations for comparison.
#' @param DF A daily streamflow data frame as from \code{ch_read_ECDE_flows}
#' @param st_date starting date format is \%Y/\%m/\%d
#' @param end_date ending date format is \%Y/\%m/\%d
#' @return Returns a data within the specific time period as a dataframe.
#' @export
#' @author Paul Whitfield  
#' @examples
#' data(W05AA008)
#' subset <- ch_cut_block(W05AA008,"2000/01/01", "2010/12/31")

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
    print( paste("incorrect date format", st_date, "must be like  2010/01/01"))
    return ()
  }

  if (!st_date >= min(DF$Date)){
    print(paste("Starting Date",st_date, "is before records are available"))
    return()
  }

  if (!end_date <= max(DF$Date)){
    print(paste("Ending Date",end_date, "is after records are available"))
    }

  result1 <- DF[DF$Date >= st_date, ]
  result <- result1[result1$Date <= end_date, ]
  
  print(paste("between",st_date,"and", end_date, length(result[ , 1]),
              "records were selected"))
  
  return(result)
}
