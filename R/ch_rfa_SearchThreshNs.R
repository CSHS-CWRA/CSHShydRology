#' @export
#' @rdname SearchThresh
SearchThreshNs <- 
  function(form, x, tau,
           nmin = 20, verbose = TRUE, newdata = NULL, ...){

  tau <- sort(unique(tau))

  if(length(tau) < 2)
    stop('More than one threshold must be specified.')

  ntau <- length(tau)

  ans <- matrix(NA, ntau, 12)
  rt <- c(2,5,10,20,50,100)
  rt.name <- paste0('q',rt)
  colnames(ans) <- c('u', 'n', 'ppy', 'mrl', 'kappa', rt.name,'ad')
  
  resp <- eval(form[[2]], x) 

  bar <- txtProgressBar()
  for(ii in 1:ntau){

    if(verbose)
      setTxtProgressBar(bar, ii/ntau)

    fit0 <- try(FitNsPot(form, x, tau = tau[ii], ...))

    ## If there was an error
    if(class(fit0) == 'try-error')
      next

    suppressWarnings(gof0 <- GofTest(fit0)$pvalue)

    pred0 <- predict(fit0, rt = rt, reliability = TRUE, newdata = newdata)

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

  ans <- as.data.frame(ans)
    
  ## Compute the false discovery rate for the remaining points
  Fun <- function(z) min(p.adjust(ans$ad[z:length(ans$ad)], 'fdr'))
  ans$fdr <- sapply(seq_along(ans$ad), Fun)
  
  return(ans)
}