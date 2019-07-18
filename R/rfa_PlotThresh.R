#######################################################################
#' Visual diagnostic for peaks over threshold
#'
#' Create various graphics to assess the selection of a threshold.
#' Includes a plot with respect oh the threshold for the p-values for the 
#' Anderson-Darling test, estimated parameter and flood quantiles.
#' Vertical lines are shown for thresholds associated to 1 and 1.5 peaks per 
#' years.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param obj Output from \link{SearchThresh}.
#' 
#' @param type Type of plot to display. 
#'   Must be one or more of \code{'ad','mrl','alpha', 'kappa'}.
#'   
#' @param ppy Range of peaks per year to display.
#' 
#' @param ... Other parameters.
#'   
#' @seealso \link{SearchThresh}, \link{FindThresh}. 
#'
#' @export
#'
#' @examples
#'
#' lstu <- seq(500,2000,20)
#' out <- SearchThresh(flow~date, flowStJohn, u = lstu, declust = 'wrc', r = 14)
#'
#' PlotThresh(out)
#'
#' PlotThresh(out, type = 'q50')
#'
#' PlotThresh(out, type = paste0('q',c(2,5,10,20,50,100)))
#'
#' par(mfrow = c(2,2))
#' PlotThresh(out, type = c('ad','kappa', 'mrl','q10'))
#'
PlotThresh <- function(obj, type = 'ad', ppy = NULL, ...){

  u1 <- obj$u[which.min(abs(obj$ppy-1))]
  u15 <- obj$u[which.min(abs(obj$ppy-1.5))]

  ## Filter the threshold with proper PPY
  if(!is.null(ppy)){
    pid <- x$ppy >= ppy[1] & x$ppy <= ppy[2]

    if(any(pid)){
      x <- x[pid,]
    } else{
      stop('There is no threshold found in the PP range.')
    }
  }

  ## ------------------------------------------------
  ## Create a graphic of the Anderson-Darling p-value
  ## ------------------------------------------------

  if('ad' %in% type ){
    plot(ad ~ u, obj, type = 'l',
         ylab = 'Anderson-Darling (p-value)',
         xlab = 'Threshold')
    abline(h = c(0.05, 0.25), lty = 3)
    abline(v = c(u15, u1), col = c('blue','red'))
  }

  ## ------------------------------------------------
  ## Create a graphic of Mean residual life
  ## ------------------------------------------------

  if('mrl' %in% type ){
    lb <- obj$mrl - 2*obj$mrl_se
    ub <- obj$mrl + 2*obj$mrl_se

    plot(mrl ~ u, obj, type = 'l',
         ylim = c(.95,1.05) * c(min(lb),max(ub)),
         ylab = 'Mean residual Life',
         xlab = 'Threshold')
    lines(obj$u, lb, lty = 3)
    lines(obj$u, ub, lty = 3)
    abline(v = c(u15, u1), col = c('blue','red'))
  }

  ## ------------------------------------------------
  ## Create a graphic of the shape parameter
  ## ------------------------------------------------

  if('kappa' %in% type ){
    lb <- obj$kappa - 2*obj$kappa_se
    ub <- obj$kappa + 2*obj$kappa_se

    plot(kappa ~ u, obj, type = 'l',
         ylim = c(.95,1.05) * c(min(lb),max(ub)),
         ylab = 'Parameter (kappa)',
         xlab = 'Threshold')
    lines(obj$u, lb, lty = 3)
    lines(obj$u, ub, lty = 3)
    abline(v = c(u15, u1), col = c('blue','red'))
  }

  ## ------------------------------------------------
  ## Create a graphic of the scale parameter
  ## ------------------------------------------------

  if('alpha' %in% type ){
    lb <- obj$alpha - 2*obj$alpha_se
    ub <- obj$alpha + 2*obj$alpha_se

    plot(alpha ~ u, obj, type = 'l',
         ylim = c(.95,1.05) * c(min(lb),max(ub)),
         ylab = 'Parameter (alpha)',
         xlab = 'Threshold')
    lines(obj$u, lb, lty = 3)
    lines(obj$u, ub, lty = 3)
    abline(v = c(u15, u1), col = c('blue','red'))
  }

  ## ------------------------------------------------
  ## Create a graphic of a flood quantile
  ## ------------------------------------------------

  qname <- paste0('q',c(2,5,10,20,50,100))
  qq <- qname %in% type

  if(sum(qq) == 1){

    qlab <- qname[which(qq)]
    qlab.se <- paste0(qlab,'_se')
    q0 <- obj[,qlab]
    qse0 <- obj[,qlab.se]
    lb <- q0 - 2 * qse0
    ub <- q0 + 2 * qse0

    plot(obj$u, q0, type = 'l',
         ylim = c(.95,1.05) * c(min(lb),max(ub)),
         ylab = paste0('Flood quantile(',toupper(qlab),')'),
         xlab = 'Threshold')
    lines(obj$u, lb, lty = 3)
    lines(obj$u, ub, lty = 3)
    abline(v = c(u15, u1), col = c('blue','red'))
  }

  ## ------------------------------------------------
  ## Create a graphic of multiple flood quantiles
  ## ------------------------------------------------

  if(sum(qq) > 1){

    qname0 <- qname[which(qq)]
    xq <- obj[,qname0]

    lb <- min(xq[,1])
    ub <- max(xq[,ncol(xq)])

    plot(obj$u, xq[,1], type = 'l',
         ylim = c(.95,1.05) * c(min(lb),max(ub)),
         ylab = 'Flood quantiles',
         xlab = 'Threshold')
    text(obj$u[1], xq[1,1], labels = toupper(qname0[1]), pos = 3)

    for(ii in 2:ncol(xq)){
      lines(obj$u, xq[,ii])
      text(obj$u[1], xq[1,ii], labels = toupper(qname0[ii]), pos = 3)
    }
    abline(v = c(u15, u1), col = c('blue','red'))
  }

}
