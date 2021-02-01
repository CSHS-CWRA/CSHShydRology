#' Fitting a regional model by likelihood techniques.
#' 
#' Return the fitting regional of a multiple sites based on likelihood 
#' techniques.
#'
#' @param x Data with times in rows and sites in columns.
#' 
#' @param distr Fitting distribution: \code{('gev','gpa','glo','gno','pe3')}.
#' 
#' @param type Type of regional models to be fitted. Either ones of 
#' \code{'mean'}, \code{'cv'}, \code{'shape'}.
#' See the details section.
#' 
#' @param index A scale factor to standardize each sites. 
#' 
#' @param para0 Initial parameter estimates. 
#' 
#' @param ... Other parameters to pass to the \link{optim} function.
#'
#' @return
#' 
#' \item{index}{Site-specific parameter.}
#' \item{para}{Regional parameters.}
#' \item{llik}{The sum of the independent log-likelihood.}
#' \item{type}{Type of regional model.}
#' \item{na}{Indices of the missing values.}
#' \item{dim}{Dimension of the input data.} 
#' 
#' @references 
#' 
#' * Hosking, J. R. M., & Wallis, J. R. (1997). Regional frequency analysis: 
#' an approach based on L-moments. Cambridge Univ Pr.
#' 
#' * Padoan, S. A., Ribatet, M., & Sisson, S. A. (2010). Likelihood-Based 
#' Inference for Max-Stable Processes. Journal of the American Statistical 
#' Association, 105(489), 263-277. https://doi.org/10.1198/jasa.2009.tm08577
#' 
#' @details 
#' 
#' Different regional models can be selected by the argument \code{type}. 
#' All models assumed the same distribution for all sites, which must be one of
#' the followings: GEV, GLO, GNO, PE3 and GPA. 
#' Please see Hosking and Wallis (1997) for more details on the distributions.
#' All distributions have a location, scale and shape parameter.
#' One exception is the GPA distribution for which the location parameter is 
#' known to be zero.
#' The regional models combine local and regional parameters to fully 
#' characterize each at-site distribution.
#' All sites are estimated jointly by maximizing the sum of the log-likelihood
#' of the individual sites.
#' 
#' For \code{type = 'mean'} an index-flood model is estimated by assuming that all 
#' distributions are proportional up to a scale factor. 
#' By default the scale factor is the at-site mean, but can be replaced 
#' by other quantities as passed to \code{index}. 
#' The likelihood techniques is then used to estimate the regional parameters.
#' 
#' The option \code{type = 'shape'} assume that site has a common shape 
#' parameter and the remaining parameters are site-specific. 
#' For \code{type = 'cv'} the only the location parameters are site-specific.
#' Therefore, the scale parameters are set as proportional to 
#' the location parameters up to a regional parameter 
#' that has similar a interpretation as a coefficient of variation.
#' For this regional model, a constant shape parameter is also assumed for all
#' sites. 
#' 
#' It should be pointed out that the independent log-likelihood can be used to 
#' select the distributions when it has the same number of parameters. 
#' Consequently, GPA should not be compared to the other distributions and  the
#' same type of model should be used.
#' 
#' 
#' @seealso \link{FitPoolMargin} and \link{predict.poolmle}.
#' @export
#'
#' @examples
#' 
#' xd <- flowAtlantic$ams 
#' ## transpose by year
#' xd$year <- format(xd$date, '%Y')
#' xw <- DataWide(ams ~ id + year, xd)
#' 
#' ## Fitting a regional shape parameter
#' fit.gev <- FitPoolMle(xw, 'gev', type = 'shape')
#' fit.gno <- FitPoolMle(xw, 'gno', type = 'shape')
#' print(fit.gev)
#' 
#' ## In this case GEV is a better model. 
#' print(fit.gev$llik)
#' print(fit.gno$llik)
#' 
#' ## Return all the parameters of the marginal distribution.
#' coef(fit.gev)
#' 
#' ## Exemple of index-flood model estimated by MLE
#' fit <- FitPoolMle(xw, 'gev', type = 'cv')
#' print(fit)
#' 
#' 
FitPoolMle <- function(x, distr = 'gev', type = 'mean', 
                       index = NULL, para0 = NULL, ...){
  
  ## Verify the choice of the selected distribution
  if(!(distr %in% c('gpa', 'gev', 'glo', 'gno', 'pe3')))
    stop('Must provide a valid distribution')
  
  ## If an index-flood factor is passed the regional model must be the same
  if(!is.null(index))
    type <- 'mean'
  
  ## Transform data into list
  xl <- as.list(as.data.frame(x))
  xl <- lapply(xl, na.omit)
    
  ## Extract the density function
  dfun <- getFromNamespace(paste0('d',distr),'CSHShydRology')
  
  ## Fitting the model
  if(type == 'mean'){
    suppressWarnings(
      ans <- .FitPoolMle.mean(x = xl, distr = distr, index = index,
                                   para0 = para0, dfun = dfun, ...))
  } else if(type == 'cv'){
    
    if(distr == 'gpa')
      stop('A 3 parameters distribution must be used with this regional model')
    
    suppressWarnings(
      ans <- .FitPoolMle.cv(x = xl, distr = distr, para0 = para0, dfun = dfun, ...))
    
  } else if(type == 'shape' & distr == 'gpa'){
    suppressWarnings(
      ans <- .FitPoolMle.pot(x = xl, distr = distr, dfun = dgpa, ...))
    
  } else if(type == 'shape' & distr != 'gpa'){

    suppressWarnings(
      ans <- .FitPoolMle.shape(x = xl, distr = distr, para0 = para0, dfun = dfun, ...))
  }
  
  ## Format the output
  ans$type  <- type
  ans$distr <- distr
  ans$na <- which(is.na(x))
  ans$dim <- dim(x)
  
  class(ans) <- 'poolmle'
  
  return(ans)
}

.FitPoolMle.mean <- function(x, distr, index = NULL, para0, dfun, ...){
  
  ## If not provided, extract the index-flood for each site
  if(is.null(index)){
      index <- sapply(x, mean)
  } 
  
  ## Standardize the data
  x <- mapply('/', x, index, SIMPLIFY = FALSE)
    
  ## Built a function that return the neg. log-likelihood for one site
  if(distr == 'gpa'){
    nllik.site <- function(z, para)
      -sum(dfun(z, exp(para[1]), para[2], log = TRUE))
  
  } else {
    nllik.site <- function(z, para)
      -sum(dfun(z, para[1], exp(para[2]), para[3], log = TRUE))
  }
  
  ## Built a function that return the independent negative log-likelihood
  nllik.ind <- function(para){
    sum(sapply(x, nllik.site, para))
  }
    
  ## If not provided, provide starting parameters
  if(is.null(para0) & distr == 'gpa'){
    para0 <- c(-1.2, -.1)
  } else if(is.null(para0)){
    para0 <- lmomco::lmom2par(lmomco::vec2lmom(c(1,.3,.23)), distr)$para
    para0[2] <- log(para0[2])
  }
  
  ## Estimation of the parameters
  sol <- try(optim(para0, nllik.ind, ...), silent = TRUE)
  
  if(class(sol) == 'try-error'){
    para0[2] <- -para0[2] 
    sol <- optim(para0, nllik.ind, ...)
  }
  
  if(sol$convergence != 0)
    warning('Solution may have not converged')
  
  ## Prepar the final ouput
  ans <- list(index = index,
              para = sol$par,
              llik = -sol$value)
  
  if(distr == 'gpa'){
    names(ans$para) <- c('alpha', 'kappa')
    ans$para[1] <- exp(ans$para[1])

  }else{
    
    names(ans$para) <- c('xi', 'alpha', 'kappa')
    ans$para[2] <- exp(ans$para[2])
  }
  
  return(ans)
}

.FitPoolMle.pot <- function(x, distr, ...){
  
  ## extract the index-flood for each site
  rg <- lapply(x,range)
  rg <- lapply(rg, log)
  
  ## Function that return the negative log-likelihood of one site
  nllik.site <- function(scl, z, shp){
    -sum(dgpa(z, exp(scl), shp, log = TRUE))
  }
  
  ## Function that return the optimal scale parameter for one site
  ## knowing the shape parameter
  Fscl <- function(scl, z, shp){
    optimize(nllik.site, scl, z = z, shp = shp)$minimum 
  }

  ## Function that return the nllik of all site (independent)
  nllik.reg <- function(shp){
    plst <- list(shp = shp)
    scl <- mapply(Fscl, rg, x, MoreArgs = plst)
    sum(mapply(nllik.site, scl, x, MoreArgs = plst))
  }
  
  ## Estimate the parameters
  suppressWarnings(
    sol <- optimize(nllik.reg, c(-.5,1)))
  
  index <- exp(mapply(Fscl, rg, x, MoreArgs = list(shp = sol$minimum)))
  
  ## Prepar the final ouput
  ans <- list(index = index,
              para = sol$minimum,
              llik = -sol$objective)
  
  names(ans$para) <- 'kappa'
  
  return(ans)
}

.FitPoolMle.cv <- function(x, distr, dfun, para0 = NULL, ...){
  
  ## extract the index-flood for each site
  cv <- mean( sapply(x,sd) / sapply(x, mean))
  rg <- lapply(x,range)
  
  ## Function that return the negative log-likelihood of one site
  nllik.site <- function(loc, z, scl, shp){
    -sum(dfun(z, loc, loc * exp(scl), shp, log = TRUE))
  }
  
  Floc <- function(z, rg, scl, shp){
    optimize(nllik.site, rg, z = z, scl = scl, shp = shp)$minimum
  }
  
  nllik.reg <- function(p){
    plst <- list(scl = p[1], shp = p[2])
    loc <- mapply(Floc, x, rg, MoreArgs = plst)
    sum(mapply(nllik.site, loc = loc, z = x,  MoreArgs = plst))
  }
  
  if(is.null(para0))
    para0 <- c(log(cv), -.1)
  
  ## Estimate the parameters
  sol <- try(optim(para0, nllik.reg, ...), silent = TRUE)
  sol <- optim(para0, nllik.reg)
  
  if(class(sol) == 'try-error' ){
    para0[2] <- .1
    sol <- try(optim(para0, nllik.reg, ...), silent = TRUE)
  }
  
  if(sol$convergence != 0)
    warning('Solution may have not converged')
    
  plst <- list(scl = sol$par[1], shp = sol$par[2])
  index <- mapply(Floc, x, rg, MoreArgs = plst)
  
  ## Prepar the final ouput
  ans <- list(index = index,
              para = sol$par,
              llik = -sol$value)
              
  ans$para[1] <- exp(ans$para[1])
  
  names(ans$para) <- c('cv','kappa')
  
  return(ans)
}


.FitPoolMle.shape <- function(x, distr, dfun, para0 = NULL, ...){
  
  paras <- as.data.frame(sapply(x,fAmax, distr))
  paras <- paras[-3,]
  paras[2,] <- log(paras[2,])
  
  ## Function that return the negative log-likelihood of one site
  nllik.site <- function(p, z, shp){
    -sum(dfun(z, p[1], exp(p[2]), shp, log = TRUE))
  }
  
  Fp <- function(z, p, shp){
   optim(nllik.site, par = p, z = z , shp = shp)$par
  }
  
  nllik.reg <- function(shp){
    p <- mapply(Fp, p = paras, z = x, 
                MoreArgs = list(shp = shp), SIMPLIFY = FALSE)
    sum(mapply(nllik.site, p = p, z = x,  MoreArgs = list(shp = shp)))
  }
  
  ## Find an initial set of shape parameter
  if(is.null(para0))
    ini.seq <- seq(-.5,.5,.05)
  else
    ini.seq <- seq(para0[1],para0[2],para0[3])
  
  suppressWarnings(
    ini <- lapply(ini.seq, function(z) try(nllik.reg(z), silent = TRUE)))
  
  ini.id <- which(sapply(ini,class) != 'try-error')
  
  para0 <- range(ini.seq[ini.id])
  
  ## Estimate the parameters
  sol <- optimize(nllik.reg, para0)
  
  index <- mapply(Fp, p = paras, z = x, 
                MoreArgs = list(shp = sol$minimum))
  
  index[2,] <- exp(index[2,])
  
  ## Prepar the final ouput
  ans <- list(index = index,
              para = sol$minimum,
              llik = -sol$objective)
  
  names(ans$para) <- 'kappa'
  
  return(ans)
}


#' @export
print.poolmle <- function(x, ...){
  cat('\nRegional model for pooling groups\n')
  
  cat('\nType: ', x$type, sep = '')
  cat('\nNumber of sites:', length(x$index))
  cat('\nInd. loglik:', round(x$llik, digits = 2))
  
  cat('\nDistribution:', x$distr)
  cat('\nParameters\n')
  print(x$para, digits = 4)
  
}

#' @export
coef.poolmle <-function(object, ...){
 
  nsite <- object$dim[2]
  
  if(object$type == 'mean'){
    ans <- replicate(nsite, object$para)
    
  } else if(object$type == 'cv'){
    
    ans <- rbind(object$index, 
                 object$index * object$para[1], 
                 object$para[1])
    
  } else if(object$type == 'shape'){
    ans <- rbind(object$index, object$para)
  } 
  
  ## format the output
  if(object$distr == 'gpa')
    rownames(ans) <- c('alpha','kappa')
  else
    rownames(ans) <- c('xi','alpha','kappa')
  
  if(object$type == 'shape' & object$distr != 'gpa')
    colnames(ans) <- colnames(object$index) 
  else
    colnames(ans) <- names(object$index) 
  
  return(ans)
}
