##############################################################################
#' Fit POT models for a list of candidate thresholds
#'
#' The function \code{SearchThresh} returns a data frame containing
#' key value for the automatic selection of a threshold and where each row
#' corresponds to a candidate threshold.
#' The result is extracted from several calls to the function \code{FitPot}.
#' The function \code{SearchThreshNs} perform the similar search except that 
#' it used the nonstationary model fitted by \cite{FitNsPot}. 
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x,form Dataset and formula passed to \link{FitPot} or \cite{FitNsPot}.
#'
#' @param u,tau Vector of candidate thresholds.
#' 
#' @param nmin Stopping condition verifying that a minimal number of peaks
#'   are extracted.
#' 
#' @param verbose Logical. Should a progress bar be displayed.
#' 
#' @param ... Other arguments passed to \link{FitPot} or \cite{FitNsPot}.
#'
#' @seealso \link{FitPot}, \cite{FitNsPot}, \cite{FindThresh}
#' 
#' @export
#'
#' @examples
#'
#' data(flowStJohn)
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
#' ## Search for nonstationary model
#' 
#' tau <- seq(.93, .98, .005)
#' taus <- SearchThreshNs(flow~date, flowStJohn, tau = tau, trend = ~1, thresh = ~1,
#'                       declust = 'wrc', r = 14)
#'                       
#' FindThresh(us, method = 'sgn-max')
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

    fit0 <- try(FitPot(form, x, u = u[ii], ...))

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

##############################################################################

#' @export
#' @rdname SearchThresh
SearchThreshNs <- 
  function(form, x, tau,
           nmin = 20, verbose = TRUE, ...){

  tau <- sort(unique(tau))

  if(length(tau) < 2)
    stop('More than one threshold must be specified.')

  ntau <- length(tau)

  ans <- matrix(NA, ntau, 12)
  rt <- c(2,5,10,20,50,100)
  rt.name <- paste0('q',rt)
  colnames(ans) <- c('tau', 'n', 'ppy', 'mrl', 'kappa', rt.name,'ad')
  
  resp <- eval(form[[2]], x) 

  bar <- txtProgressBar()
  for(ii in 1:ntau){

    if(verbose)
      setTxtProgressBar(bar, ii/ntau)

    fit0 <- try(FitNsPot(form, x, tau = tau[ii], ...))

    ## If there was
    if(!is.nspot(fit0))
      next

    suppressWarnings(gof0 <- GofTest(fit0)$pvalue)

    pred0 <- predict(fit0, rt = rt, reliability = TRUE)

    ans[ii,] <- c(tau[ii],                            # threshold
                  length(fit0$peak),                  # number of excess
                  length(fit0$peak)/fit0$nyear,       # Peaks per year
                  mean(residuals(fit0, 'thresh')),    # Mean residual life
                  fit0$kappa,                         # parameter
                  pred0,                              # flood quantiles
                  gof0)                               # AD p-value

    if(length(fit0$peak) < nmin){
      nu <- ii
      ans <- ans[1:nu,]
      break
    }
  }

  return(as.data.frame(ans))
}
