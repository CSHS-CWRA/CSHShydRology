#' Stacks EC values
#' 
#' @description Converts data frames of ECCC year x month or 
#' month x day data to vectors.
#' @param data_values Required. Data frame of year x month or month x day values.
#' @param data_codes Required. Data frame of year x month or month x day data codes.
#'
#' @return Returns a data frame with two colums: the data values, and the data codes.
#' @export
#' @keywords internal
#' @author Kevin Shook
#'
#' @examples \dontrun{df <- ch_stack_EC(data_values, data_codes)}
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

#' @description Subsets a data frame by an specified date range, provided as
#' a string by the \code{prd} argument. This function is meant to emulate the subsetting
#' capability of the \pkg{xts} package.
#'
#' @param df data frame of time series data; includes a variable called \code{Date}
#' @param prd date range as string formatted as \option{YYYY-MM-DD/YYYY-MM-DD}
#' @return \item{df}{subsetted data frame}
#' @keywords date data subset
#' @author Robert Chlumsky <rchlumsk@gmail.com>
#' @export
#' @examples{
#' dd <- seq.Date(as.Date("2010-10-01"), as.Date("2013-09-30"), by = 1)
#' x <- rnorm(length(dd))
#' y <- abs(rnorm(length(dd)))*2
#' df <- data.frame("Date" = dd,x,y)
#' prd <- "2011-10-01/2012-09-30"
#' summary(ch_date_subset(df,prd))}

ch_date_subset <- function(df, prd) {
  ss <- unlist(strsplit(prd, split = "/"))
  df <- df[df$Date >= as.Date(ss[1]) & df$Date <= as.Date(ss[2]), ]
  return(df)
}
