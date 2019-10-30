##############################################################################
#' Prediction of the nonstationary model for AMAX using regression.
#' 
#' Return the flood quantiles associatied to specifc level of 
#' exceeding probability or reliability level. 
#' The function \code{BootNsAmax} is used to obtain bootstrap samples of 
#' the parameters and flood quantiles.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param object Output form \link{FitNsAmax}.
#' 
#' @param p Exceeding probability associated to a quantile or reliability level.
#'    
#' @param newdata Covariates. If \code{NULL} the input data that served to fit 
#'   the model is used
#'   
#' @param reliability Logical. Should the flood quantile associated with 
#'   reliability level be returned. In that case, \code{newdata} must represent
#'   the period of interest. See Durocher et al. (2019) for more details.
#' 
#' @param ... Other parameters.
#' 
#' @details 
#' 
#' The reliability is defined as the probability that there is no event that 
#' will exceed as given design level for a specific period. 
#' When \code{reliability = TRUE}, the elements \code{newdata} are used to define
#' that period. For instance the last 30 years can be obtained by passing the
#' the last 30 records of the fitted dataset. The reliability level is control 
#' by \code{p} and is \code{p^s} where \code{s} is the size of \code{newdata}.
#' 
#' @references 
#' 
#' Durocher, M., Burn, D. H., & Ashkar, F. (2019). Comparison of estimation 
#'   methods for a nonstationary index-flood model in flood frequency analysis 
#'   using peaks over threshold [Preprint]. 
#'   https://doi.org/10.31223/osf.io/rnepc
#' 
#' @seealso \link{FitNsAmaxMle}, \link{FitNsPot}, \link{predict.nsamax}.
#'
#' @export
#'
#' @examples
#' 
#' data(flowStJohn)
#' 
#' amax <- ExtractAmax(flow~date, flowStJohn, tol = 365)
#' 
#' ## Recenter year to start at 1
#' amax$year <- as.integer(amax$yy) - 1926
#' 
#' fit <- FitNsAmax(flow~year, amax, 'gev', type = 'mult')
#' 
#' ## Last 30 years
#' ref.period <- 59:88
#' 
#' hat <- predict(fit, c(.5, .9))
#' rel <- predict(fit, c(.5, .9), newdata = amax[ref.period,], reliability = TRUE)
#' 
#' ## Data cloud
#' plot(flow~year, amax)
#' 
#' ## Time-depedent return level
#' for(ii in 1:2) lines(hat[,ii], col = 'red')
#' 
#' ## Reliability levels for last 30 years
#' for(ii in 1:2) lines(cbind(ref.period, rel[ii]), lwd = 3, col = 'blue')
#' 
#' ## Using boostrap to evaluate the model uncertainty
#' bs <- BootNsAmax(fit, c(.5, .9), nsim = 50, 
#'                  newdata = amax[ref.period,], reliability = TRUE)
#' summary(bs)
#' 
#' ## Return a single simulation of the model
#' simulate(fit)
#' 
predict.nsamax <- 
  function(object, 
           p = c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99),
           newdata = NULL,
           reliability = FALSE, ...){
    
  if(is.null(newdata))
    newdata <- object$data
    
  if(!reliability){
    ans <- .predict.nsamax.rt(object, p, newdata)
    
    if(ncol(ans) == 1){
      ans <- as.vector(ans)
      names(ans) <- rownames(newdata)
      
    } else if(nrow(ans) == 1){
      ans <- as.vector(ans)
      names(ans) <- round(p,3)
      
    } else {
      colnames(ans) <- round(p,3)
      rownames(ans) <- rownames(newdata)
    }
    
  } else {
    ans <- .predict.nsamax.rel(object, p, newdata)
    names(ans) <- round(p,3)
    
  }
  

  
  return(ans)  
}

.predict.nsamax.rel <- function(object, p, newdata){
 
  ## Evaluate trend
  xmat <- model.matrix(attr(object$data,'terms'), newdata)
  mu <- as.numeric(xmat %*% object$beta)
  
  if(object$type == 'mult')
    mu <- exp(mu)
  
  ## evaluate the reliability level
  rel <- log(p) * nrow(newdata)

  ## evaluate the quantile using the index-flood equation
  q0 <- lmomco::qlmomco(p, object$para)

  if(object$type == 'add'){
    Frel <- function(z, jj) rel[jj]-sum(log(lmomco::plmomco(z-mu, object$para)))
    q0 <-  outer(mu, q0, '+')
  
  } else if(object$type == 'mult'){
    Frel <- function(z, jj) rel[jj]-sum(log(lmomco::plmomco(z/mu, object$para)))
    q0 <- outer(mu, q0, '*')
  }
  
  ## determine Bound for searching
  bnd <- apply(q0, 2, range)  
  bnd[1,] <- bnd[1,] - sqrt(.Machine$double.eps)
  bnd[2,] <- bnd[2,] + sqrt(.Machine$double.eps)
  
  ans <- rep(0,length(rel))
  for(jj in seq_along(rel))
    ans[jj] <- uniroot(Frel, bnd[,jj], jj)$root
  
  return(ans)
}

.predict.nsamax.rt <- 
  function(object, 
           p = c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99),
           newdata = NULL){
    
  xmat.new <- model.matrix(attr(object$data,'terms'), newdata)
  
  if(object$type == 'mult'){
      zhat <- lmomco::qlmomco(p,object$para)
      yhat <- as.numeric(exp(xmat.new %*% object$beta))
      ans <- outer(yhat, zhat, '*')
    
  } else if(object$type == 'add'){
      zhat <- lmomco::qlmomco(p, object$para)
      yhat <- as.numeric(xmat.new %*% object$beta)
      ans <- outer(yhat, zhat, '+')
  }
  
  return(ans)
}