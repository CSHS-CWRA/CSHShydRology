#' @export
#' @rdname FitRegLmom
plot.reglmom <- function(x, xlab = 'L-Skew', ylab = 'L-kur', 
                         line.distr = c('glo','gev','gno','pe3','gpa'), legend.do = TRUE,
                         legend.args = NULL, ...){
  
  ## Scatter plot of the L-ratios
  lmm <- x$lmom[,3:4]
  colnames(lmm) <- c(xlab, ylab)
  plot(lmm, ...)
  points(x$rlmom[3],x$rlmom[4], pch = 16, col = 'red', cex = 1.5)
  
  ## Add theoritical lines
  LmomDiag(line.distr)
  
  ## Add legend
  if(legend.do){
   
    if(is.null(legend.args$color))
      legend.args$col <- seq_along(line.distr)
    
    if(is.null(legend.args$legend))
      legend.args$legend <- line.distr
    
    if(is.null(legend.args$x))
      legend.args$x <- 'topleft'
    
    if(is.null(legend.args$lty))
      legend.args$lty <- rep(1,length(line.distr))
     
    do.call(graphics::legend, legend.args)
  }  
}