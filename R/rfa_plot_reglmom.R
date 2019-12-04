#' @export
#' @rdname FitRegLmom
plot.reglmom <- function(x, ...){
  lmm <- x$lmom[,3:4]
  colnames(lmm) <- c('t_3','t_4')
  lmom::lmrd(lmm, ...)
  points(x$rlmom[3],x$rlmom[4], pch = 16, col = 'red', cex = 1.5)
}