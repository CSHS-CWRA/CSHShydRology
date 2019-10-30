#' @export
predict.poolmargin <-function(object, p = c(.5, .8, .9, .95, .98, .99), ...){
  
  if(object$distr == 'gpa'){
    ans <- apply(object$para,2, function(z) qgpa(p, z[1],z[2]))  
  
  } else {
    qfun <- getFromNamespace(paste0('q',object$distr), 'CSHShydRology')
    ans <- apply(object$para, 2, function(z) qfun(p, z[1],z[2], z[3]))
  }

  if(is.matrix(ans))
    rownames(ans) <- round(p,3)
  
  return(ans)
}