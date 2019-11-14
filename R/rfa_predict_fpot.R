###############################################################
#' Prediction of flood quantiles of a given return period.
#'
#' Return a vector or matrix of the flood quantiles, its standard deviation and 
#' confidence intervals.  
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' 
#' @param object Output of \link{FitPot}.
#' 
#' @param rt Return period. 
#' 
#' @param se Logical. Should the standard deviation be returned.
#' 
#' @param ci,alpha Method for evaluation the confidence intervals with 
#'   probability \code{1-alpha}. Available methods are : 
#'   Delta method (\code{'delta'}), profile likelihood (\code{'profile'}) 
#'   and nonparametric bootstrap (\code{'boot'})
#'   
#' @param nsim Number of bootstrap sample.
#' 
#' @param out.matrix Logical. Should the bootstrap sample be returned?
#' 
#' @param ... Other parameters.
#' 
#' @seealso \link{FitPot}
#' 
#' @references 
#' 
#' Coles S. (2001) An introduction to statistical modeling of extreme values.
#'   Springer Verlag.
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
#' predict(fit, se = TRUE, ci = 'delta')
#' 
predict.fpot <- 
  function(object, 
           rt = c(2, 5, 10, 20, 50, 100), 
           se = FALSE,
           ci = "none", 
           alpha = 0.05, 
           nsim = 500,
           out.matrix = FALSE,
           ...){

  ## funtion that compute the return period
  Fpred <- function(para, lambda){

    if(abs(para[2]) > sqrt(.Machine$double.eps))
      ans <- object$u + para[1]/para[2] * (1-lambda^(-para[2]) )
    else
      ans <- object$u + para[1] * log(lambda)

    return(ans)
  }

  lambda <- object$nexcess * rt * object$unit / object$ntot
  hatRt <- Fpred(object$estimate, lambda)

  ## Standard error by delta method
  if(se | ci %in% c('delta','profile')){
    negk <- -object$estimate[2]
    lk <- lambda^negk
    ak <- object$estimate[1]/negk

    gx1 <-  object$estimate[1] * (object$ntot/object$nexcess)  * lk
    gx2 <- (lk-1)/negk
    gx3 <-  ak * ( lk * log(lambda) - gx2)

    gx <- rbind(gx1, gx2, gx3)
    hatSe <-  sqrt(diag(t(gx) %*% vcov(object, rate = T) %*% gx))
  }

  ## Confident interval by delta method
  if(ci == 'delta'){
     lb <- hatRt - qnorm(1-alpha/2) * hatSe
     ub <- hatRt + qnorm(1-alpha/2) * hatSe
  

  ## Confident interval by nonparametric bootstrap
  } else if(ci == 'boot'){

    ## Resample
    nexcess <- rpois(nsim,object$nexcess)
    Fsim <- function(l, p) rgpa(l, p[1], p[2])
    xboot <- lapply(nexcess, Fsim, object$estimate)
    
    ## Define the m-observation return level
    lambdas <- lapply(nexcess, '*', rt * object$unit / object$ntot) 
    
    if(object$method == 'lmom')
      fn <- fgpaLmom
    else if(object$method == 'mom')
      fn <- fgpaMom
    else if(object$method == 'mle2')
      fn <- fgpa2d
    else if (object$method == 'mle')
      fn <- fgpa1d
    
    pboot <- lapply(xboot, fn)
    qboot <- mapply(Fpred, pboot, lambdas)

    ## Compute interval
    if(class(qboot) == 'numeric')
      qboot <- t(qboot)
    
    bootci <- t(apply(qboot, 1, quantile, c(alpha/2, 1-alpha/2)))
    lb <- bootci[,1]
    ub <- bootci[,2]
    
    pboot <- do.call(rbind, pboot)
    qboot <- t(qboot)
    colnames(qboot) <- paste0('Q',rt)

  

  ## Confident interval by profile likelihood
  } else if(ci == 'profile'){

    bnd0 <- c(-.5,1)

    ## Bound of the deviance
    khi <- qchisq(1-alpha,1)
    lb <- lb0 <- pmax(1e-8, hatRt - 10 * hatSe)
    ub <- ub0 <- hatRt + 10 * hatSe

    for(ii in seq_along(rt)){

      ## Deviance function in respect of the return period
      Dfun <- function(z){

        ## Full likelihood
        llikFull <- sum(dgpa(object$excess, object$estimate[1],
                                         object$estimate[2], log = TRUE))

        ## Negative log-likelihood
        nllik <- function(p){

          num <- z - object$u

          if(abs(p) < 1e-8)
            denum <- log(lambda[ii])
          else
            denum <- -(lambda[ii]^(-p) -1)/p

          return(-sum(dgpa(object$excess, pmax(1e-8,num/denum), p, log = TRUE)))

        }

        ## Compute the profile likelihood
        suppressWarnings(llik0 <- -optimize(nllik, bnd0)$objective)
        return(2*(llikFull-llik0))

      }

      ## Lower bound
      suppressWarnings(lb[ii] <- optimize(function(z) abs(Dfun(z) - khi),
                                          c(lb0[ii],hatRt[ii]))$minimum)

      ## Upper bound
      suppressWarnings(ub[ii] <- optimize(function(z) abs(Dfun(z) - khi),
                                          c(hatRt[ii], ub0[ii]))$minimum)

    } # end for

  } # end if

  ## Does ci was computed
  ci <- !(ci=='none')

  ## construct the output
  if(se & !ci)
    ans <- data.frame(pred = hatRt, se = hatSe)
  else if(!se & ci)
    ans <- data.frame(pred = hatRt, lower = lb, upper = ub)
  else if(se & ci)
    ans <- data.frame(pred = hatRt, se = hatSe, lower = lb, upper = ub)
  else
    ans <- data.frame(pred = hatRt)


  ## Format the output
  if(ncol(ans) == 1){
    ans <- ans[,1]
  } else {
    rownames(ans) <- as.character(rt)
  }
  
  if(out.matrix)
    ans <- list(pred = ans, para = pboot, qua = qboot)

  return(ans)
}