#' @export
coef.nsamax.mle <- function(object, ...){
  
  xmat <- model.matrix(attr(object$data,'term'), object$data)
  np <- ncol(xmat)
  
  loc <- as.numeric(xmat %*% object$para[1:np])
  scl <- object$para[np+1]

  if(object$type == 'mult')
    scl <- scl * abs(loc)
  
  shp <- object$para[np+2]
  
  ans <- suppressWarnings(data.frame(xi = loc, alf = scl, kap = shp))
  
  if(object$distr == 'pe3')
    colnames(ans) <- c('mu','sigma','g')
  
  return(ans)
}