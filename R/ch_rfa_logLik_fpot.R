#' @export
#' @rdname FitPot
logLik.fpot <- function(object, ...)
  sum(dgpa(object$excess, object$estimate[1], object$estimate[2], log = TRUE))
