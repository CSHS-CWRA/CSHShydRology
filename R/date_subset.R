#' Subset date by String

#' This function subsets a data frame by an specified date range, provided as
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
#' summary(date_subset(df,prd))}

date_subset <- function(df, prd) {
  ss <- unlist(strsplit(prd, split = "/"))
  df <- df[df$Date >= as.Date(ss[1]) & df$Date <= as.Date(ss[2]), ]
  return(df)
}
