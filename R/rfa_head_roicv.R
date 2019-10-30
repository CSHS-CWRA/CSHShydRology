#' @export
#' @rdname CvRoi
head.roicv <- function(x, crit = 'mad', ...){

  lstCrit <- c('rmse','rrmse','nsh','mad','rmad','smad')
  bid <- which(crit == lstCrit)+1
  
    ## Find the best row
  if(crit %in% c('nsh','smad')){
    best <- order(x[,bid], decreasing = TRUE)[1:3]
  } else{
    best <- order(x[,bid])[1:3]
  }
  

  cat('\nCross-validation for Region of Influence (ROI)\n')
  cat('\nBest 3 sizes of neighborhood\n')
  ans <- as.matrix(x[best,])
  print(ans)

}