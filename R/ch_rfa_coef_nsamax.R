#' @export
#' @rdname fitted.nsamax
#' @noRd
coef.nsamax <- function(object, ...) 
  c(object$beta, object$para$para)
