#' ch_color_gradient
#'
#' @description set colour gradient
#'
#' @param x  array of variable
#' @param colors an array of colours to form a gradient
#' @param climits provide specific limits for common scaling



ch_color_gradient <- function(x, colors=c("darkred", "red","white","green", "darkgreen"), colsteps = 100, climits = NULL) {
  if(is.null(x)) stop("x is NULL")
if(is.null(climits)) return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out = colsteps)) ] )
if(!is.null(climits)) return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(climits[1],climits[2], length.out = colsteps)) ] )
}
