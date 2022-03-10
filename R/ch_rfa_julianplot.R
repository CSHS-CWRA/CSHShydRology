#' Circular plotting by day of year 
#'
#' @description Create axis for plotting circular statistics in a unitary circle.
#'
#' @author Martin Durocher
#' 
#' @param rose.col,rose.lwd,rose.cex Properties of the polar axes.
#' 
#' @param rose.radius Vector of the position of the circular axis.
#' 
#' @param ... Other parameter passed to \link{points}.
#' 
#' @seealso \link{ch_rfa_seasonstat}.
#'
#' @export
#' 
#' @importFrom graphics segments
#' 
#' @return Returns a empty rose plot by day of year
#'
#' @examples
#'
#' data(flowAtlantic)
#' 
#' ss <- ch_rfa_seasonstat(date ~ id, flowAtlantic$ams)
#'
#' ch_rfa_julianplot()
#' points(y ~ x, ss, pch = 16, col = cut(ss[,'radius'], c(0,.5,.75,1)))
#'
ch_rfa_julianplot <- function(rose.col = "gray40", rose.lwd = 1.5,
                       rose.cex = 1.5, rose.radius = seq(.25,1,.25), ...){

  plot(1, pch = '', ylim = c(-1,1)*1.2, xlim = c(-1,1)*1.2, axes = FALSE,
       ylab = "", xlab = "")

  DrawCircle(0,0, radius = rose.radius, col = rose.col, lwd = rose.lwd)

  TextCircle(month.abb, radius = 1.1, col = rose.col, cex = rose.cex)

  segments(0,-1,0,1, col = rose.col, lwd = rose.lwd)
  segments(-1,0,1,0, col = rose.col, lwd = rose.lwd)

}


DrawCircle <- function(x = 0, y = NULL, radius = 1, res = 500, ...){

  if (inherits(x, "data.frame") | inherits(x, "maxtrix")) { 
    y <- x[,2]
    x <- x[,1]
  } else if (inherits(x, "list")) {
    y <- x$y
    x <- x$x
  } else if (inherits(x, "formula")) {
    xd <- model.frame(x,y)
    y <- x[,2]
    x <- x[,1]
  }

  if (is.null(y)) stop('Locations not correctly specified')

  ## Series of angles
  tt <- c(seq(0,2*pi, len = res),0)

  if (length(x) == length(y) & length(y) == length(radius)) {
    for (ii in seq_along(radius))
        lines(radius[ii]*cos(tt) + x[ii],
              radius[ii]*sin(tt) + y[ii], type = 'l' ,...)

  } else {
    for (ii in seq_along(radius))
      lines(radius[ii]*cos(tt) + x[1],
            radius[ii]*sin(tt) + y[1], type = 'l' ,...)
  }
}


TextCircle <- function(label, x = 0, y = 0, radius = 1, ...){

  pp <- 1/(length(label))

  for (ii in seq_along(label)) {
    ang <- 2*pi*pp*(ii - 1)
    xii <- radius*cos(ang) + x
    yii <- radius*sin(ang) + y
    text(xii, yii,labels = label[ii],...)
  }
}
