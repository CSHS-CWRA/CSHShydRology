#' Creates a colour gradient
#'
#' @description Creates a colour gradient for plotting.
#'
#' @param x  Vector of values used for gradient.
#' @param colors Vector of colours to form a gradient. Default is \code{`c("darkred", "red","white","blue", "darkblue")`}.
#' @param colsteps The number of steps in the gradient. Default is \code{100}.
#' @param climits Sets specific limits for common scaling.
#'
#' @return \item{res}{returned array of colour codes}
#' @author modified by Paul Whitfield
#' @export
#' 
#' @examples
#  # plot randomly distributed data
#' plot(rnorm(20),col='black')
#'
#' # create a red blue colour gradient for plotting
#' mycol <- ch_col_gradient(rnorm(20), colsteps = 100)
#'
#' # plot more random points in transparent blue colour
#' points(rnorm(20), col = mycol)

ch_col_gradient <- function(x, colors=c("darkred", "red","white","blue", "darkblue"), colsteps = 100, climits = NULL) {
  if (is.null(x)) stop("x is NULL")
  if (is.null(climits)) 
    return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out = colsteps)) ] )
   if (!is.null(climits)) 
     return(colorRampPalette(colors) (colsteps) [ findInterval(x, seq(climits[1],climits[2], length.out = colsteps)) ] )
}
