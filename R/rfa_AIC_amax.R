#' @export
AIC.amax <- function(object, k = 2, ...)
  as.numeric(k*length(object$para) - 2*object$llik)