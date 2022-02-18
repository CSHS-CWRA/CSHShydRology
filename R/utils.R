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
#' @examples \donttest{df <- ch_stack_EC(data_values, data_codes)}
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

#' @title Adjusts colour codes to introduce transparency
#'
#' \code{ch_col_transparent} is used to adjust colour codes to introduce transparency.
#'
#' @param colour Vector of colours you wish to add transparency to.
#' @param trans Integer(s) describing the degree of transparency, from ~200
#' (slightly transparent) to <10 (very transparent).
#' @return \item{res}{returned updated colour code with transparency}
#' 
#' @seealso See original code on post in Stack Overflow
#' \href{http://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-rmaking}{
#' plot points transparent in R}
#' 
#' @keywords colour transparency
#' @examples
#'
#' # plot randomly distributed data
#' plot(rnorm(20),col='black')
#'
#' # create a transparent blue colour for plotting
#' mycol <- ch_col_transparent('blue',100)
#'
#' # plot more random points in transparent blue colour
#' points(rnorm(20),col=mycol)
#' 
#' # add transparency to multiple colours
#' ch_col_transparent( c('red','blue','green'), c(50,100,200))
#'
#' @importFrom grDevices col2rgb
#' @export ch_col_transparent
ch_col_transparent <- function(colour, trans)
{
  
  if (length(colour) != length(trans) & !any(c(length(colour),length(trans)) == 1)) stop("Vector lengths not correct")
  if (length(colour) == 1 & length(trans) > 1) colour <- rep(colour,length(trans))
  if (length(trans) == 1 & length(colour) > 1) trans <- rep(trans,length(colour))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split = ""))
    return(paste(hex[(x - x %% 16)/16 + 1],hex[x %% 16 + 1],sep = ""))
  }
  rgb <- rbind(col2rgb(colour),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse = ""),sep = "")
  return(res)
}
