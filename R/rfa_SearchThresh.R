##############################################################################
#' Fit POT models for a list of candidate thresholds
#'
#' The function \code{SearchThresh} returns a data frame containing
#' key value for the automatic selection of a threshold and where each row
#' corresponds to a candidate threshold.
#' The result is extracted from several calls to the function \code{FitPot}.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x,form Dataset and formula passed to \link{FitPot}.
#'
#' @param u Vector of candidate thresholds.
#' 
#' @param nmin Stopping condition verifying that a minimal number of peaks
#'   are extracted.
#' 
#' @param verbose Logical. Should a progress bar be displayed.
#' 
#' @param ... Other arguments passed to \link{FitPot}.
#'
#' @export
#'
#' @examples
#'
#' # Create a list of candidate threshold
#' lstu <- which.floodPeaks(flow~date, flowStJohn, u =500, r = 14)
#' lstu <- sort(unique(flowStJohn[lstu,'flow']))
#' lstu <- lstu[seq(1,length(lstu)-30,2)]
#'
#' out <- SearchThresh(flow~date, flowStJohn, u = lstu, declust = 'wrc', r = 14)
#' head(out)
#'
#' FindThresh(out, method = 'sgn', tol.sgn = 0.1)
#' FindThresh(out, method = 'ppy', tol.ppy = 2)
#' FindThresh(out, method = 'max')
#'
SearchThresh <- function(form, x, u, nmin = 20, verbose = TRUE, ...){

  u <- sort(unique(u))

  if(length(u) < 2)
    stop('More than one threshold must be specified.')

  nu <- length(u)

  ans <- matrix(NA, nu, 23)
  qq <- c(2,5,10,20,50,100)
  qname <- paste0('q',qq)
  colnames(ans) <- c('u', 'n','nyear', 'ppy','mrl', 'mrl_se',
                     'alpha','kappa', 'alpha_se', 'kappa_se',
                     qname, paste0(qname,'_se'),
                     'ad')

  bar <- txtProgressBar()
  for(ii in 1:nu){

    if(verbose)
      setTxtProgressBar(bar, ii/nu)

    fit0 <- try(FitPot(form, x, u = u[ii], declust = 'wrc', r = 14))

    if(class(fit0) != 'fpot')
      next

    gof0 <- GofTest(fit0)$pvalue

    pred0 <- predict(fit0, se = TRUE)

    ans[ii,] <- c(u[ii],                              ## threshold
                  fit0$nexcess,                       ## number of excess
                  fit0$nyear,                         ## number of years
                  fit0$nexcess/fit0$nyear,            ## Peaks per year
                  fit0$mrl,                           ## Mean residual life
                  sd(fit0$excess)/sqrt(fit0$nexcess),  ## sdev
                  fit0$estimate,                      ## parameter
                  sqrt(diag(fit0$varcov)),            ## sdev
                  pred0[,1],                          ## flood quantiles
                  pred0[,2],                          ## sdev
                  gof0)                               ## AD p-value

    if(fit0$nexcess < nmin){
      nu <- ii
      ans <- ans[1:nu,]
      break
    }
  }

  ans <- as.data.frame(ans)

  return(ans)
}


