#' Stacks EC values
#' 
#' @description Converts data frames of Environment Canada year x month or 
#' month x day data to vectors.
#' @param data_values Required. Data frame of year x month or month x day values.
#' @param data_codes Required. Data frame of year x month or month x day data codes.
#'
#' @return Returns a data frame with two columns: the data values, and the data codes.
#' @export
#' @keywords internal
#' @author Kevin Shook
#'
#' @examples \dontrun{
#' # Do not run as the function requires a data frame of EC data and
#' # the dummy variable will cause an error message
#' df <- ch_stack_EC(data_values, data_codes)
#' }
#' 
ch_stack_EC <- function(data_values = NULL, data_codes = NULL) {
  #check parameters
  if (is.null(data_values))  {
    stop("No specified data values")
  }
  
  if (is.null(data_codes)) {
    stop("No specified data codes")
  }
  
  # transpose data
  data_values_t <- t(data_values)
  data_codes_t <- t(data_codes)
  
  # now stack data frames to vectors
  data_values <- as.vector(data_values_t, mode = 'numeric')
  data_codes <- as.character(as.vector(data_codes_t, mode = 'character'))
  
  df <- data.frame(data_values, data_codes)
  return(df)
}

 
#' Subsets dates by string
#' 
#' @description Subsets a data frame by an specified date range, provided as
#' a string by the \code{prd} argument. This function is meant to emulate the subsetting
#' capability of the \pkg{xts} package.
#'
#' @param df data frame of time series data; includes a variable called \code{Date}
#' @param prd date range as string formatted as \option{YYYY-MM-DD/YYYY-MM-DD}
#' @return \item{df}{subsetted data frame}
#' @keywords date data subset
#' @author Robert Chlumsky
#' @export
#' @examples{
#' dd <- seq.Date(as.Date("2010-10-01"), as.Date("2013-09-30"), by = 1)
#' x <- rnorm(length(dd))
#' y <- abs(rnorm(length(dd)))*2
#' df <- data.frame("Date" = dd,x,y)
#' prd <- "2011-10-01/2012-09-30"
#' summary(ch_date_subset(df,prd))}
#' 
ch_date_subset <- function(df, prd) {
  ss <- unlist(strsplit(prd, split = "/"))
  df <- df[df$Date >= as.Date(ss[1]) & df$Date <= as.Date(ss[2]), ]
  return(df)
}


#' Tests url to see if it will work
#'
#' @param url Required. URL to be checked
#' @param quiet Optional. If \code{FALSE} (the default) messages are printed.
#'
#' @return Returns \option{error} if there was an error, \option{warning} if there was a
#' warning. Otherwise, returns \option{OK}. Strings are returned instead of logical values
#' to simplify checking result in calling function.
#' @seealso See original code on post in Stack Overflow
#' \href{https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r}{
#' How to write trycatch in R}
#' @export
#' @keywords internal
#' @author Kevin Shook
#'
#' @examples \donttest{
#' # Not tested automatically as can be very slow
#' test_url <- "https://zenodo.org/record/4781469/files/sm_data.csv"
#' ch_test_url_file(test_url, quiet = TRUE)
#' }
#' 
ch_test_url_file <- function(url, quiet = FALSE){
    out <- tryCatch(
      {
        readLines(con = url, n = 1, warn = FALSE) 
      },
      error = function(cond) {
        if (!quiet) {
          message(paste("URL does not seem to exist:", url))
          message("Here's the original error message:")
          message(cond)
        } else{
        }

        # Choose a return value in case of error
        return("error")
      },
      warning = function(cond) {
        if (!quiet) {
          message(paste("URL caused a warning:", url))
          message("Here's the original warning message:")
          message(cond)
          # Choose a return value in case of warning
        } else{
        }

        return("warning")
      },
      finally = {
        if (!quiet) {
          message(paste("Processed URL:", url))
        } else {
        }

      }
    ) 
    if (out != "error" & out != "warning")
      out <- "OK"
    
    return(out)
  }