###############################################################
#' Prediction of flood quantiles of a given return period.
#'
#' Return a vector or matrix of the flood quantiles, its standard deviation and 
#' confidence intervals.  
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' 
#' @param obj Output of \link{FitPot}.
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
predict.fpot <- function(obj, rt = c(2, 5, 10, 20, 50, 100), se = FALSE,
                          ci = "none", alpha = 0.05, nsim = 1000){

  ## Define the m-observation return level
  lambda <- rt * obj$unit * obj$nexcess / obj$ntot

  ## funtion that compute the return period
  Fpred <- function(para){

    if(abs(para[2]) > 1e-8)
      ans <- obj$u + para[1]/para[2] * (1-lambda^(-para[2]) )
    else
      ans <- obj$u + para[1] * log(lambda)

    return(ans)
  }

  hatRt <- Fpred(obj$estimate)

  ## Standard error by delta method
  if(se | ci %in% c('delta','profile')){
    negk <- -obj$estimate[2]
    lk <- lambda^negk
    ak <- obj$estimate[1]/negk

    gx1 <-  obj$estimate[1] * (obj$ntot/obj$nexcess)  * lk
    gx2 <- (lk-1)/negk
    gx3 <-  ak * ( lk * log(lambda) - gx2)

    gx <- rbind(gx1, gx2, gx3)
    hatSe <-  sqrt(diag(t(gx) %*% vcov(obj, rate = T) %*% gx))
  }

  ## Confident interval by delta method
  if(ci == 'delta'){
     lb <- hatRt - qnorm(1-alpha/2) * hatSe
     ub <- hatRt + qnorm(1-alpha/2) * hatSe
  }

  ## Confident interval by nonparametric bootstrap
  if(ci == 'boot'){

    ## Resample
    xboot <- replicate(nsim, sample(obj$excess, obj$nexcess, replace = TRUE))

    ## Refit and predict
    if(obj$method == 'lmom')
      fn <- function(z) Fpred(fgpaLmom(z))
    else if(obj$method == 'mle2')
      fn <- function(z) Fpred(fgpa2d(z))
    else if (obj$method == 'mle')
      fn <- function(z) Fpred(fgpa1d(z))

    pboot <- apply(xboot,2,fn)

    ## Compute interval
    bootci <- t(apply(pboot, 1, quantile, c(alpha/2, 1-alpha/2)))
    lb <- bootci[,1]
    ub <- bootci[,2]

  }

  ## Confident interval by profile likelihood
  if(ci == 'profile'){

    bnd0 <- c(-.5,1)

    ## Bound of the deviance
    khi <- qchisq(1-alpha,1)
    lb <- lb0 <- pmax(1e-8, hatRt - 10 * hatSe)
    ub <- ub0 <- hatRt + 10 * hatSe

    for(ii in seq_along(rt)){

      ## Deviance function in respect of the return period
      Dfun <- function(z){

        ## Full likelihood
        llikFull <- sum(dgpa(obj$excess, obj$estimate[1],
                                         obj$estimate[2], log = TRUE))

        ## Negative log-likelihood
        nllik <- function(p){

          num <- z - obj$u

          if(abs(p) < 1e-8)
            denum <- log(lambda[ii])
          else
            denum <- -(lambda[ii]^(-p) -1)/p

          return(-sum(dgpa(obj$excess, pmax(1e-8,num/denum), p, log = TRUE)))

        }

        ## Compute the profile likelihood
        llik0 <- -goptimize(nllik, bnd0)$objective
        return(2*(llikFull-llik0))

      }

      ## Lower bound
      suppressWarnings(lb[ii] <-
                goptimize(function(z) abs(Dfun(z) - khi),
                         c(lb0[ii],hatRt[ii]))$minimum)

      ## Upper bound
      suppressWarnings(ub[ii] <-
                         goptimize(function(z) abs(Dfun(z) - khi),
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

  return(ans)
}


#################################
# One Dimensional Optimization
#
# Perform an initial search inside a grid of points before performing
# standard optimization routine.
#
# param res Number of subdivisions.
# param ... See basic function optimize

goptimize <- function(fn, interval, res = 20, ...){

  igrid <- seq(interval[1],interval[2], len = res)
  id <- which.min(sapply(igrid,fn))

  if(id == 1) bnd0 <- igrid[1:2]
  else if(id == res) bnd0 <- igrid[c(res-1,res)]
  else bnd0 <- igrid[c(id-1,id)]

  return(optimize(fn, bnd0, ...))
}
