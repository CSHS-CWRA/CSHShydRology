#' @export
predict.nsamax.mle <- 
  function(object, 
           p = c(.5, .8, .9, .95, .98, .99), 
           newdata = NULL, 
           type = 'quantile',
           se = FALSE){
  
  if(is.null(newdata))
    newdata <- object$data
  
  newdata <- as.data.frame(newdata)

  ########################################
  ## Return trend or location component ##
  ########################################
  
  xmat <- model.matrix(object$formula, newdata)
  np <- ncol(xmat)
  
  loc <- as.numeric(xmat %*% object$para[1:np])

  if(type == 'location')
    return(loc)
  
  ################################
  ## Return the scale component ##
  ################################
  
  scl <- rep(object$para[np+1], nrow(xmat))
  names(scl) <- NULL

  if(object$type == 'mult')
    scl <- scl * abs(loc)
  
  if(type == 'scale')
    return(scl)
  
  shp <- object$para[np+2]
  
  #########################
  ## return the quantile ##
  #########################
  
  Fqua <- getFromNamespace(paste0('q',distr),'CSHShydRology')
  qua <- sapply(p, Fqua, loc, scl, shp)
  colnames(qua) <- as.character(round(p,3))
  rownames(qua) <- rownames(xmat)
  
  if(type == 'quantile')
    return(qua)
  
  #################
  ## reliability ##
  #################
  
  if(type != 'reliability')
    stop('Must select a valid type.')
  
  Fcdf <- getFromNamespace(paste0('p',distr),'CSHShydRology')
  
  lp <- log(p) * nrow(newdata)
  
  Frel <- function(z, jj) lp[jj]-sum(log(Fcdf(z, loc, scl, shp)))

  bnd <- apply(qua, 2, range)
  
  rel <- rep(0,length(lp))
  for(jj in seq_along(lp))
    rel[jj] <- uniroot(Frel, bnd[,jj], jj)$root
  
  names(rel) <- as.character(round(p,3))
  return(rel)
  }




#' @export
BootNsAmax.mle <- 
  function(object,
           p = c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99),
           newdata = NULL,
           reliability = FALSE,
           nsim = 1000, 
           alpha = 0.95, 
           verbose = TRUE, ...){
  
  if(is.null(newdata))
    newdata <- object$data  
  
  ## allocate memory
  paras <- matrix(0, nsim, length(object$para))
  
  if(reliability){
    quas <- matrix(0, nsim, length(p))
  } else {
    quas <- array(0, dim = c(nrow(newdata), length(p), nsim))
  }
  
  xjj <- object$data
  
  
  if(verbose)
    bar <- txtProgressBar()
  
  for(jj in 1:nsim){
    
    if(verbose)
      setTxtProgressBar(bar, jj / nsim)
    
    ## Simulate a bootstrap sample
    xjj[,1] <- simulate(object)
    
    ## Fit the model
    fit <- try(FitNsAmaxMle(form = object$formula, x = xjj, varcov = FALSE,
                     distr = object$distr, type = object$type, ...)) 
    
    ## Return missing if fails
    if(class(fit) == 'try-error'){
      paras[jj,] <- NA
          
      if(reliability){
        quas[jj,] <- NA
      } else{
        quas[,,jj] <- NA
      }
      
      next  
    }
    
    ## save results
    paras[jj,] <- fit$para
    
    if(reliability){
      quas[jj,] <- predict(fit, p, newdata, 'reliability')
    } else{
      quas[,,jj] <- predict(fit, p, newdata, 'quantile')
    }
  }    
  
  colnames(paras) <- names(fit$para)
  colnames(quas) <- as.character(round(p,3))
  
  ans <- list(para = paras,
              qua = quas)
  
  class(ans) <- 'bootnsamax'
  
  return(ans)
}
