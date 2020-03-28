##########################################################################
#' Estimating intersite correlation
#'
#' Return a matrix of intersite correlation between paired observations.
#' By default the empirical matrix is estimated and corrected to be 
#' positive definite. 
#' Can also be estimated by a power exponential model where the weighted least 
#' squares approach is used with weights proportional to record lengths.
#'  
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Dataset in the wide format
#'
#' @param type Model type. Can be either \code{'emp'} for estimating
#'   the empirical correlation matrix or \code{'exp'} 
#'   for fitting power exponential model.
#'   
#' @param method Measure of association used to evaluate dependence. 
#'   Either \code{'spearman'} or \code{'kendall'}.
#'
#' @param distance Matrix of distances. Necessary when using \code{'exp'}.
#'
#' @param nmin Minimal number of pairs necessary to compute pairwise
#'   correlations.
#'
#' @param na.sub Value for imputing missing pairwise correlation with
#'   \code{'emp'}.
#'
#' @param defpos Logical. Should a correction be applied to the empirical
#'   matrix to ensure positive definiteness.
#'
#' @param smooth Smooth parameter of the power exponential model.
#'
#' @param distance.max  Maximal distance to consider for paired observations
#'   in the fitting of the power exponential model.
#'   
#' @param distance.bin Number of equally space bins used in the fitting of the
#'   of the power exponential model.
#'
#' @param start Initial parameter for optimization with the \code{exp} method.
#'   It must have the form \code{(nugget, range)}.
#' 
#' @param ... Other parameters.
#'
#' @return
#' \describe{
#'   \item{method}{Estimation method.}
#'   \item{para}{Parameter of the fitted model. For \code{'emp'} it
#'               is the average.}
#'
#'   \item{corr}{Raw estimation of the correlations.}
#'   \item{model}{Final estimate of the correlations.}
#'   \item{rmse}{For \code{'exp'}, root mean square errors of the fitted model.}
#' }
#'
#' @section References:
#'
#' Durocher, M., Burn, D.H., Mostofi Zadeh, S., 2018. A nationwide
#'  regional flood frequency analysis at ungauged sites using ROI/GLS with
#'  copulas and super regions. Journal of Hydrology 567, 191-202.
#'  https://doi.org/10.1016/j.jhydrol.2018.10.011
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(flowAtlantic)
#' 
#' ## Organize annual maximums
#' ams <- flowAtlantic$ams
#' ams$year <- format(ams$date, '%Y')
#' xmat <- DataWide(ams ~ id + year, ams)
#' 
#' ## Compute distance between sites
#' coord <- flowAtlantic$info[, c('lon','lat')]
#' rownames(coord) <- flowAtlantic$info$id
#' h <- GeoDist(coord)
#' 
#' ## make sure that columns match
#' h <- h[colnames(xmat), colnames(xmat)]
#'
#' ## estimate intersite correlation using a model
#' isite <- Intersite(xmat, distance = h, type = 'exp')
#' print(isite)
#' 
#' ## Evaluate emprirical correlation matrix
#' isite <- Intersite(xmat[,1:5])
#' print(isite)
#' round(isite$model,2)
#' }
#' 
Intersite <-
  function(x,
           type = 'emp',
           method = 'spearman',
           distance = NULL,
           nmin = 5,
           na.sub = 'avg',
           defpos = TRUE,
           smooth = 1,
           distance.max = Inf,
           distance.bin = 15,
           start = NULL){


  if(!is.matrix(x))
    stop('Must be a matrix with sites in columns')

  ## Compute correlation coefficient derived from Spearman's rho or Kendall tau
  corr <- cor(x, use = 'pairwise.complete.obs', method = method)
  
  if(method == 'spearman'){
    corr <- 2*sin(pi/6*corr)
  } else if(method == 'kendall'){
    corr <- sin(pi/2*corr)
  } else {
    stop('Must choose a valid measure of association.') 
  }
    
  ## Compute the number of paired observations for each site
  nmat <- crossprod(!is.na(x))
  
  ## Flag sites with too few pairwise observations
  cid <- (nmat < nmin)
  if (any(cid))
    corr[cid] <- NA

  if (type == 'exp'){
    ## --------------------------------------------
    ## Fit power exponential model for correlation
    ## --------------------------------------------

    if (is.null(distance))
      stop('Distance must be specified for smoothing correlation')

    distance <- as.matrix(distance)

    if(any(colnames(distance) != colnames(x)))
      warning('Sites may not matched with distance matrix')

    ##-----------------------------------------------------------##
    ## Fit a power exponential model on the pairwise correlation
    ##-----------------------------------------------------------##

    ## extract pairs
    pcor <- corr[lower.tri(corr)]
    pdist <- distance[lower.tri(distance)]
    w <- nmat[lower.tri(nmat)]
    
    ## remove pairs too far
    pid <- (pdist <= distance.max)
    pcor <- pcor[pid]
    pdist <- pdist[pid]
    w <- w[pid]

    ## remove NA
    pid <- !is.na(pcor)
    pcor <- pcor[pid]
    pdist <- pdist[pid]
    w <- w[pid]


    ## binning
    brks <- quantile(pdist, seq(0,1, len = distance.bin + 1))
    brks.bin <- as.integer(cut(pdist, brks))
    
    pdist.mean <- tapply(pdist, brks.bin, mean)
    pcor.mean <- tapply(pcor, brks.bin, mean)
    w.sum <- tapply(pcor, brks.bin, sum)
    
    ## Define power exponential model
    Fcor <- function(p,h){
      ## ensure positiveness
      p[1] <- exp(p[1])/(1 + exp(p[1]))
      p[2] <- exp(p[2])

      return(p[1] * exp(-3*( h / p[2])^smooth))
    }
    
    ## Define MSE of the correlation fit
    Fobj <- function(p) sum(w.sum *(pcor.mean - Fcor(p, pdist.mean))^2)

    ## Set a starting value
    if(is.null(start)){
      start = c(2.2,log(mean(pdist)))
    } else{
      start = c(log(start[1]/(1-start)), log(start[2]))
    }

    ## Optimize the MSE
    sol <- optim(start, Fobj)

    rmse <- sqrt(sol$value)

    ## Create the new smooth correlation matrix base on fitted model
    corr.model <- Fcor(sol$par,distance)
    diag(corr.model) <- 1

    ## Extract the parameters of the correlation model
    para <- c(1-exp(sol$par[1])/(1+exp(sol$par[1])),
              exp(sol$par[2]),
              smooth)

    names(para) <- c('nugget','range','smooth')


  } else if(type == 'emp') {

    ##-----------------------------------------------------------##
    ## Correct the empirical correlation matrix
    ##-----------------------------------------------------------##
    pcor <- na.omit(corr[lower.tri(corr)])
    para <- mean(pcor)
    rmse <- NULL
    corr.model <- corr

    ## define a value for imputation
    if(na.sub == 'zero'){
      na.sub <- 0
    } else if(na.sub == 'avg'){
      na.sub <- para[1]
    } else if(na.sub == 'NA'){
      na.sub <- NA
      defpos <- FALSE ## no correction if there is NA
    }

    #impute missing values
    cid <- is.na(corr.model)
    if (any(cid))
      corr.model[cid] <- na.sub

    ## force the diagonal to 1
    diag(corr.model) <- 1

    ## If necessary, correct to obtain a positive definite matrix
    if(defpos)
      corr.model <- as.matrix(Matrix::nearPD(corr.model, corr = TRUE)$mat)

  }

  ## Final output
  ans <- list(type = type,
              method = method,
              corr = corr,
              model = corr.model,
              para = para,
              rmse = rmse)
  
  if(type == 'exp'){
    ans$bin <- data.frame(x = pdist.mean, 
                          y = pcor.mean,
                          w = w.sum)
  }

  class(ans) <- 'isite'

  return(ans)
}
