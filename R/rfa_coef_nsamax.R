#' @export
#' @rdname fitted.nsamax
coef.nsamax <- function(object, ...) 
  c(object$beta, object$para$para)
