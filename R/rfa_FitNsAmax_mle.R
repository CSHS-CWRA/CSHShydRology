#' @export
FitNsAmaxMle <- 
  function(form, 
           x, 
           distr, 
           type = 'add', 
           varcov = TRUE,
           method = 'BFGS',
           control = NULL,
           ini = NULL, 
           quiet = FALSE,...){
  
  ## Extract the response variable and design matrix
  xd <- get_all_vars(form, x)
  resp <- eval(form[[2]], env = xd)
  xmat <- model.matrix(form, xd)
  np <- ncol(xmat)
  
  ## Extract the pdf function
  if(distr %in% c('gev','glo','gno','pe3')){
    Fpdf <- getFromNamespace(paste0('d',distr), 'CSHShydRology')
  } else {
    stop('The selected distribution is not supported')
  }
    
  ## Function that evaluate the negative log-likelihood 
  ## Note the optional log transformation for scale parameter
  nllik <- function(p, transf = TRUE){
    loc <- as.numeric(xmat %*% p[1:np])
    shp <- p[np+2] 
    
    if(transf)
      scl <- exp(p[np+1])
    else
      scl <- p[np+1]
    
    if(type == 'mult')
      scl <- scl * abs(loc)
    
    -sum(Fpdf(resp, loc, scl, shp, log = TRUE))
  }
  
  ## initial values if not provided
  if(is.null(ini)){
    beta0 <- lsfit(xmat, resp, intercept = F)$coefficients
    
    if(type == 'add'){
      alf0 <- sd(resp - xmat %*% beta0)
      
    } else if (type == 'mult'){
      alf0 <- sd(resp / (xmat %*% beta0))
    }
    
    ini <- c(beta0, log(alf0),-.1)
      
  } else {
    ini[np+1] <- log(ini[np+1])
  }
  
  ## Estimation of the model
  sol <- optim(ini, nllik, method = method, control = control)
  sol$par[np+1] <- exp(sol$par[np+1])
  
  ## Name the scale and shape parameters
  if(distr == 'pe3'){
    names(sol$par)[np + 1:2] <- c('sig', 'g')
  } else {
    names(sol$par)[np + 1:2] <- c('alf', 'kap')
  }
  
  ## Warn from possible problem of convergence
  if(!quiet & sol$convergence != 0)
    warning('Solution have not converged')
  
  ## Should the variance-covariance be returned
  if(varcov){
    
    ## Evaluate the covariance matrix from the hessian
    h <- try(optimHess(sol$par, nllik, transf = FALSE, control = control))
    h <- try(as.matrix(Matrix::nearPD(h)$mat))
    varcov <- try(chol2inv(chol(h)))
  
    if(class(varcov) == 'try-error'){
      varcov <- NA
    } else {
      colnames(varcov) <- rownames(varcov) <- names(sol$par)
    }
  }
  
  ans <- list(formula = form,
              data = xd,
              distr = distr,
              type = type,
              para = sol$par,
              varcov = varcov,
              convergence = sol$convergence,
              message = sol$message)
  
  class(ans) <- "nsamax.mle"
  
  return(ans)
}

#' @export
print.nsamax.mle <- function(obj){
  cat('\nNonstationary model for annual maximums\n')
  cat('\nMethod: MLE')
  cat('\nDistribution:',obj$distr)
  cat('\nType:', obj$type )
  cat('\nParameters\n')
  print(obj$para, digits = 4)
  
  if(!any(is.na(obj$varcov)) )
    cat('\nStd. err.\n')
    print(sqrt(diag(obj$varcov)), digits = 4)
}

#' @export
vcov.nsamax.mle <- function(object) object$varcov

#' @export
coef.nsamax.mle <- function(object){
  
  xmat <- model.matrix(object$formula, object$data)
  np <- ncol(xmat)
  
  loc <- as.numeric(xmat %*% object$para[1:np])
  scl <- object$para[np+1]

  if(object$type == 'mult')
    scl <- scl * abs(loc)
  
  shp <- object$para[np+2]
  
  ans <- suppressWarnings(data.frame(xi = loc, alf = scl, kap = shp))
  
  if(object$distr == 'pe3')
    colnames(ans) <- c('mu','sigma','g')
  
  return(ans)
}
  
#' @export
is.nsamax.mle <- function(object){
  any(class(object) == 'nsamax.mle')
}

#' @export
is.nsamax.mle <- function(object){
  any(class(object) == 'nsamax.mle')
}

#' @export
as.nsamax.mle <- function(object){
  class(object) <- 'list'
  return(object)
}

#' @export
plot.nsamax.mle <- 
  function(obj, 
           xlab = 'Theoritical normal cumulative distribution',
           ylab = 'Empirical normal cumulative distribution',
           line.col = 'black', line.lty = 1, line.lwd = 1,
           ...){
  
  resp <- eval(form[[2]], env = obj$data)
  para <- coef(obj)
  
  Fcdf <- getFromNamespace(paste0('p',distr),'CSHShydRology')
  
  p <- Fcdf(resp, para[,1], para[,2], para[1,3])
  u <- seq_along(p)/(length(p)+1)
  plot(qnorm(u), qnorm(sort(p)), xlab = xlab, ylab = ylab, ...)
  abline(0,1, col = line.col, lty = line.lty, lwd = line.lwd)
  
}

#' @export
simulate.nsamax.mle <- function(object){

  ## Simulate a bootstrap sample
  para <- coef(object)
  Fqua <- getFromNamespace(paste0('q',object$distr), 'CSHShydRology')
  
  u <- runif(nrow(para))
  ans <- Fqua(u, para[,1], para[,2], para[1,3])
  
  return(as.numeric(ans))
}
