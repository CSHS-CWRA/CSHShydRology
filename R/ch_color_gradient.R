#' ch_color_gradient
#'
#' @description set colour gradient
#'
#' @param x  array of variable
#' @param colors an array of colours to form the desired gradient. Default is 
#' ("darkred", "red", "white", "green", "darkgreen")
#' @param climits provide specific limits for common scaling
#' @param colsteps number of steps to be used in gradient, default is 100.
#' @return vector of colors
#' 
#' @author Paul Whitfield
#' @export
#' @examples
#' cxin <- c(0, 1, 1, 3, 4, 5, 10)
#' cxout <- ch_color_gradient(cxin)
#' #[1] "#8B0000" "#B50000" "#B50000" "#FF2B2B" "#FF9292"
#' #[6] "#FFF9F9" "#006400"


ch_color_gradient <- function(x, colors=c("darkred", "red","white","green", "darkgreen"), 
                              colsteps = 100, climits = NULL) {
  if(is.null(x)) stop("x is NULL")
if(is.null(climits)) return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out = colsteps)) ] )
if(!is.null(climits)) return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(climits[1],climits[2], length.out = colsteps)) ] )
}
