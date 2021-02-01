#' @export
#' @rdname AIC.nsamax
AIC.nsamax.mle <- function(object, ..., k = 2){
  np <- length(coef(object))
  return(k * np - 2*logLik(object)) 
}