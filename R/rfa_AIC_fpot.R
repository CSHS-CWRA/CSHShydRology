#' @export
#' @rdname FitPot
AIC.fpot <- function(object, k = 2, ...)
  -2 * logLik(object) + 2 * k