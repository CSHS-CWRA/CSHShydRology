#' Add Transparency to plot colours
#'
#' @description Adds transparency to a colour based on an integer between 0 and 255,
#' with  0 being fully transparent and 255 being opaque. Based on function 
#' \code{rvn_col_transparent} in package \pkg{RavenR}.
#'
#' @param colour colour that is to be made transparent, or an array of colours
#' @param trans  an integer (or array of integers) describing the degree of 
#' transparency, 0 to 255. Must be the same length as colour. Values < 10 (very transparent),
#' values > 200 (solid colour).
#'
#' @return \item{res}{returned updated colour code with transparency}
#' @export
#' @references See original code on post in Stack Overflow \url{https://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-rmaking}
#'
#' @importFrom grDevices col2rgb
#' @author Rob Chlumsky; Paul Whitfield
#' @keywords colour transparency
#'
#' @examples
#'
#'  # plot randomly distributed data
#' plot(rnorm(20), col='black')
#'
#' # create a transparent blue colour for plotting
#' mycol <- ch_col_transparent('blue', 100)
#'
#' # plot more random points in transparent blue colour
#' points(rnorm(20),col = mycol)
#' 
#'  # plot randomly distributed data
#' plot(rnorm(20), col = 'blue')
#'
#' # create two transparent colour for plotting
#' mycol <- ch_col_transparent(c('green',"red"), c(100, 200))
#'
#' # plot more random points in transparent colours
#' points(rnorm(20), col = mycol[2])
#' 
#'

ch_col_transparent <- function(colour, trans){

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
