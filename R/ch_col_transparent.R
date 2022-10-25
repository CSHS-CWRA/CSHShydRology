#' ch_col_transparent
#' 
#' ch_col_transparent is used to adjust colour codes to introduce transparency. 
#' Based on rvn_col_transparent. This function adds transparency to a colour.
#' Define transparency with an integer between 0 (fully transparent) and 255
#' (full colour).
#'  Works with either colour and trans a vector of equal length,
#'  or one of the two of length 1.
#'
#' from http://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-r
#' #'
#' @param colour colour to make transparent
#' @param trans integer describing the degree of transparency, from ~255
#' (slightly transparent) to 0 (very transparent)
#' 
#' @return \item{res}{returned updated colour code with transparency}
#' 
#' @keywords colour transparency
#' @author Robert Chlumsky; Paul Whitfield
#' 
#' @examples
#  # plot randomly distributed data
#' plot(rnorm(20),col='black')
#'
#' # create a transparent blue colour for plotting
#' mycol <- ch_col_transparent('blue',100)
#'
#' # plot more random points in transparent blue colour
#' points(rnorm(20),col=mycol)
#'

ch_col_transparent <- function(colour,trans)
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
