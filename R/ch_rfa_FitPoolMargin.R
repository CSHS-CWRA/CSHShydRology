#' Fitting marginal distributions of a pooling group
#' 
#' Returns an object containing the parameter estimates of each site sites
#' inside a pooling groups. Estimation can be performed by L-moments or 
#' maximum likelihood for the five distributions: GEV, GLO, GNO, PE3, GPA. 
#'
#' @param x Data with time in rows and sites in columns.
#' @param distr Fitting distribution
#' @param method Estimation method.
#' @param method.optim, ... Further arguments pass to \link{optim} when 
#'   maximum likelihood method is used 
#'
#' @param ... Other parameters.
#'
#' @return
#' 
#' \item{para}{Matrix of the estimated parameters.}
#' \item{distr}{Fitting distribution.}
#' \item{na}{Indices of the missing values.}
#' \item{dim}{Dimension of the input data.}
#'
#' @seealso \link{FitPoolMle}, \link{Amax}, \link{GPA}.
#'
#' @export
#' 
#' @examples
#' 
#' x <- replicate(3, rgev(100, 100, 30, 0))
#' 
#' fit <- FitPoolMargin(x, 'gev', method = 'mle')
#' 
#' print(fit)
#' predict(fit, p = c(.9, .99))
#' 
#' xx <- simulate(fit, nsim = 2, corr = .2)
#' 
#' 
FitPoolMargin <- function(x, distr, method = 'lmom', 
                          method.optim = 'BFGS', ...){
 
  x <- as.data.frame(x)
  
  if(ncol(x) < 2)
    stop('Must have more than one site')
  
  ## Verify the choice of the selected distribution
  if(!(distr %in% c('gpa', 'gev', 'glo', 'gno', 'pe3')))
    stop('Must provide a valid distribution.')
  
  ## Transform data into list
  xl <- lapply(x, na.omit)  
  
  ans <- list(para = NULL,
              distr = distr,
              na = which(is.na(x)),
              dim = dim(x))
  
  ## Compute the model parameters
  if(distr == 'gpa' & method == 'lmom'){
    ans$para <- sapply(xl, fgpaLmom)
  } else if(method == 'lmom'){
    ans$para <- sapply(xl, fAmax, distr)
  } else if(distr == 'gpa' & method == 'mle'){
    ans$para <- sapply(xl, fgpa1d)
  } else if(distr %in% c('gev','glo','gno','pe3') & method == 'mle'){
    ffun <- getFromNamespace(paste0('f',distr),'CSHShydRology')
    ans$para <- sapply(xl, ffun, method = method.optim, ...)
  } else {
    stop('Must choose a valid distribution and estimation method.') 
  }
  
  class(ans) <- 'poolmargin' 

  return(ans)
}
