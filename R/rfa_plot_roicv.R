#' @export
#' @rdname CvRoi
plot.roicv <- 
  function(x, 
           crit = 'mad', 
           best.col = 'red',
           best.pch = 16,
           best.cex = 1, ...){

  ## Find the columns of the required criteria
  lstCrit <- c('rmse','rrmse','nsh','mad','rmad','smad')
  
  bid <- which(crit == lstCrit)+1
  
  ## Find the best row
  if(crit %in% c('nsh','smad')){
    best <- which.max(x[,bid])
  } else{
    best <- which.min(x[,bid])
  }
  
  # produce the graph
  form <- as.formula(paste(crit,'~nk'))
  plot(form, x, type = 'l', ...)
  
  points(x[best,1],
         x[best,bid], 
         col = best.col, 
         pch = best.pch, 
         cex = best.cex)
}