#' Plotting the the result of the nonstationary POT model
#' 
#' Produce a graphics of the time series with time-varying threshold and 
#' mean excess.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' 
#' @param x Output from
#'
#' @param type Type of display for the observations. See \link{par}.
#' 
#' @param line.col,line.lty,line.lwd Graphical parameter for the threshold and 
#'   the trend.
#'    
#' @param peak.col,peak.pch,peak.cex Graphical parameters for the extrated peaks.
#' 
#' @param do.legend Logical. Should a legend be displayed.
#' 
#' @param ... Other parameters.
#'
#' @export
#' 
#' @examples 
#' 
#' fit <- FitNsPot(flow~date, x = flowStJohn, tau = .95,
#'                 trend = ~ poly(date,3), thresh = ~ date, 
#'                 declust = 'wrc', r = 14)
#'                 
#'                 
#' plot(fit, do.legend = FALSE)    
#'                              
plot.nspot <- 
  function(x,
           type = 'l',
           line.col = c('red','blue'),
           line.lty = c(1,1),
           line.lwd = c(2,2),
           peak.col = 'black',
           peak.pch = 1,
           peak.cex = .7,
           do.legend = TRUE,
           ...){
  
  ## plot original data
  plot(x$data[,2:1], type = type, ...)
  
  ## plotted fitted trend and threshold model
  yhat <- fitted(x)
  points(original~time, yhat, col = peak.col, pch = peak.pch, cex = peak.cex)
  
  lines(threshold~time, yhat, 
        col = line.col[1], lwd = line.lwd[1], lty = line.lty[1] )
 
   lines(trend+threshold~time, yhat, 
         col = line.col[2], lwd = line.lwd[2], lty = line.lty[2] )
  
  if(do.legend)
     legend('topleft', legend = c('threshold','mean excess'),
           col = line.col, lty = line.lty, lwd = line.lwd)
}