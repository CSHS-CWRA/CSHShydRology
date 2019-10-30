#' @importFrom lmomco dlmomco
#' @export
#' @rdname AIC.nsamax
logLik.nsamax <- function(object, ...)
  sum(log(dlmomco(residuals(object), object$para))) 