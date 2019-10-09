 #############################################
#' Return level plot for peaks over threshold
#' 
#' Create a plot of the estimated versus sample flood quantiles, where the 
#' x-axis is expressed in terms of return period.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Output from \link{FitPot}.
#'
#' @param ci Logical. Should confidence intervals be displayed. 
#'   See \link{predict.amax} with argument Delta method.
#'   
#' @param main,ylab,xlab Graphical parameters. See \code{\link{par}}.
#'
#' @param col.ci,lty.ci,lwd.ci Graphical parameters determining the 
#'   confidence intervals.
#'
#' @param ... Other graphical parameters. See \code{\link{par}}.
#'
#' @export
#'
#' @examples
#'
#' data(flowStJohn)
#' 
#' fit <- FitPot(flow~date, flowStJohn, u = 1000,
#'                declust = 'wrc', r = 14)
#' 
#' plot(fit, ci = TRUE)

plot.fpot <- 
  function(x, 
           main = 'Return level plot',
           xlab = 'Return period (year)',
           ylab = 'Flood quantiles',
           ci = FALSE,
           col.ci = 2, 
           lty.ci = 2,
           lwd.ci = 1, 
           xlim = NULL,
           ylim = NULL,
           ...){

  ## Extract data and exceedance rate
  xd <- sort(x$excess) + x$u
  para <- x$estimate
  lambda <- length(x$excess)/x$nyear

  ## Plotting positions for the labels on the x-axis
  xat <- c(0.5, 0.6, 0.7, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 5000, 10000)
  xat <- xat[xat*lambda > 1]
  zat <- -log(-log(1-1/(lambda*xat))) ## transformed scale
  
  ## Plotting positions for the theoritical line  
  line.at <- 2^seq(-5,13, l = 200)
  line.at <- line.at[line.at*lambda > 1]
  line.p <- 1-1/(lambda * line.at)
  line.zp <- -log(-log(line.p))

  ## Empirical probabilities
  nn <- length(xd)
  emp.p <- seq(nn) / (nn+1)
  emp.zp <- -log(-log(emp.p))
  
  ## Evaluate return levels
  if(ci){

    hat <- predict(x, line.at, ci = 'delta')
        
    if(is.null(ylim)){
      hat0 <- hat[line.p <= max(emp.p), ]
      ylim <- range(c(unlist(hat0), xd))
      ylim <- ylim + c(-0.05,0.05)*diff(ylim)
    }
       
  } else{
        
    hat <- data.frame(pred = predict(x, line.at))
        
    if(is.null(ylim)){
      ylim <- range(c(hat, xd))
      ylim <- ylim + c(-0.05,0.05)*diff(ylim)
    }
      
  }
        
  ## Plot empirical points
  plot(emp.zp, xd, axes = FALSE, main = main,
       ylab = ylab, xlab = xlab, ylim = ylim, ...)

  ## Custom axis
  axis(1, at = zat, labels = xat)
  axis(2)

  ## Fitted return levels
  lines(line.zp, hat[,1], col = col.ci, lwd = lwd.ci)
    
  if(ci){
    lines(line.zp, hat[,2], col = col.ci, lwd = lwd.ci, lty = 2)
    lines(line.zp, hat[,3], col = col.ci, lwd = lwd.ci, lty = 2)
  }  
      
}
