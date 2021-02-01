#' @export
#' @rdname FitRoi
predict.roi <- function(object, x, fold = 5, ...){
  
  ## Organize the cross-validation group info
  if(length(fold) == 1){
    k <- 1:fold
    fold <- sample(rep_len(k, nrow(x)))
    
  } else{
    k <- sort(unique(fold))
  }
    
  ## allocate memory
  ans <- rep(0, nrow(x))
  
  for(ii in k){
    
    if(sum(fold != ii) <= object$call$nk)
      stop('Requires more observations')
      
    ## For each cross-validation group
    validSet <- which(fold == ii)
    
    if(is.null(object$call$kriging)){
      
      ## Fit model without kriging
      hat <- FitRoi(x = x[-validSet,], 
                  xnew = x[validSet,] , 
                  nk = object$call$nk,
                  phy = object$call$phy,
                  similarity = object$call$similarity)
      
      
    } else {
      
      ## Fit model with kriging
      hat <- FitRoi(x = x[-validSet,], 
                    xnew = x[validSet,], 
                    nk = object$call$nk,
                    phy = object$call$phy,
                    similarity = object$call$similarity,
                    kriging = object$call$kriging, 
                    model = object$model)
    }
    
    ans[validSet] <- hat$pred
  }
  
  return(ans)
}