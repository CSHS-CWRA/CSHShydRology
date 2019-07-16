#############################################################
#' Mean residual life plot
#'
#' Produce a mean residual Life plot to help selecting to help
#' selecting a proper threshold.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param form,x  Formula and sample. The must be of the form.
#'
#' @param u Series of candidate thresholds.
#'
#' @param method Estimation method.
#'
#' @param alpha Confidence interal with level \code{1-alpha/2}.
#' 
#' @param display Logical. Should the graph be display. 
#'
#' @param ... Others arguments are passed to function \code{plot} and
#' \link{which.floodPeaks}
#'
#' @details
#'
#' @seealso \link{FitPot}, \link{which.floodPeaks}.
#'
#' @examples
#'
#' ## Find list of candidate thresholds
#' lstu <- seq(500,2500, len = 50)
#'
#' PlotMrl(flow~date, flowStJohn, u = lstu, declust = 'wrc', r = 14)
#'
#' x <- PlotMrl(flow~date, flowStJohn, u = lstu, declust = 'wrc', r = 14)
#' 
#' head(x)
#'
#' @export
PlotMrl <- function(form,x, u,
                    declust = NULL, 
                    r = 1, 
                    rlow = 0.75,
                    alpha = 0.05,
                    ylab = 'Mean Residual Life', 
                    xlab = 'Threshold',
                    col = 'black', lty = 1, lwd = 1,
                    col.ci = 'black', lty.ci = 3, lwd.ci = 1,
                    ylim = NULL, display = TRUE, ...){

  ## reorganize the data
  x <- model.frame(form,x)
  dt <- x[,2]
  x <- x[,1]

  u <- as.numeric(u)

  sig <- mrl <- nn <- rep(NA,length(u))
  for(ii in seq_along(u)){

    ## Declustering
    if(is.null(declust))
      cid <- x > u[ii]
    else if(declust == 'run')
      cid <- which.clusters(x, u[ii], r)
    else if( declust == 'wrc')
      cid <- which.floodPeaks(x, dt, u[ii], r = r, rlow = rlow)

    excess <- x[cid]-u[ii]
    mrl[ii] <- mean(excess)
    nn[ii] <- length(excess)
    sig[ii] <- sd(excess)/sqrt(nn[ii])

  }

  # Confident intervals
  qn <- qnorm(1-alpha/2)
  lb <- mrl - qn * sig
  ub <- mrl + qn * sig

  ## Adjusted limits
  if(is.null(ylim)) ylim <- range(c(lb,ub)) * c(.95,1.05)

  if(display){
    plot(u, mrl, xlab = xlab, ylab = ylab, ylim = ylim,
         col = col, lty = lty, lwd = lwd, ...)
    lines(u, lb, lty = lty.ci, col = col.ci, lwd = lwd.ci)
    lines(u, ub, lty = lty.ci, col = col.ci, lwd = lwd.ci)
  }

  ## Return
  invisible(data.frame(u = u, n = nn,mrl = mrl, lower = lb, upper = ub))

}