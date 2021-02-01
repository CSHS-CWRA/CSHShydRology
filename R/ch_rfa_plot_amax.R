#############################################
#' Return level plot for annual maxima
#' 
#' Create a plot of the estimated versus sample flood quantiles, where the 
#' x-axis is expressed in terms of return period.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Output from \link{FitAmax}.
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
#' x <- ExtractAmax(flow~date, flowStJohn)$flow
#'
#' fit <- FitAmax(x, distr = 'gev', method = 'mle')
#'
#' plot(fit, ci = TRUE)

plot.amax <- 
  function(x, 
           main = 'Return level plot',
           xlab = 'Return period (year)',
           ylab = 'Flood quantiles',
           ci = FALSE, 
           col.ci = 2, 
           lty.ci = 2,
           lwd.ci = 1, 
           ...){

      ## Empirical pobability
      xd <- sort(x$data)
      n <- length(xd)
      emp.p <- seq(n)/(n+1)
      
      
      ## Plotting positions for the fitted lines
      line.p <- seq(min(emp.p), max(emp.p), l = 100)
      line.zp <- -log(-log(line.p))
      line.rt <- 1-1/line.p
      

      ## Labels for the x axis
      xat <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 5000, 10000)
      zat <- -log(-log(1-1/xat))

      
      ## Plot empirical points
      plot(-log(-log(emp.p)), xd, axes = FALSE, main = main,
           ylab = ylab, xlab = xlab, ...)

      axis(1, at = zat, labels = xat)
      axis(2)
      
      ## Add Confidence intervals if necessary
      if(ci){

        bnd <- predict(x, p = line.p, se = FALSE, ci = 'delta')

        lines(line.zp, bnd[,1], col = col.ci, lwd = lwd.ci)

        lines(line.zp, smooth.spline(line.rt,bnd[,2])$y,
              lty = lty.ci, col = col.ci, lwd = lwd.ci)

        lines(line.zp, smooth.spline(line.rt,bnd[,3])$y,
              lty = lty.ci, col = col.ci, lwd = lwd.ci)

      } else{
        bnd <- predict(x, line.p, se = FALSE, ci = 'none')
        lines(line.zp, bnd, col = col.ci, lwd = lwd.ci)
      
      }
      

}
