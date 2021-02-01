#' @export
#' @rdname FitPot
vcov.fpot <- function(object, rate = FALSE, ...){

  if(rate){
    ans <- matrix(0,3,3)

    kn <- object$nexcess/object$ntot

    ans[1,1] <- kn*(1-kn)/object$ntot
    ans[2:3,2:3] <- object$varcov

  } else
    ans <- object$varcov

  return(ans)
}