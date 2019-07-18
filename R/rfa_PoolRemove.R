#' Remove heterogenous sites from a pooling group
#'
#' Return a regional model (\code{'FitRegLmom'}) where heterogenous sites
#' are removed in a stepwise manner.
#' At each step, the removed site that best improves the homogeneity statistic
#' is permanently removed until a stopping criterion.
#' The first site that was passed in a dataset is considered as the target and
#' it cannot be removed.
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param obj An output from \link{FitRegLmom}.
#'
#' @param method,tol Which heterogenity statistics used in the procedure.
#'   The choices are \code{'H1'}, \code{'H2'} and \code{'H3'}. 
#'   The algorithm stop when the heterogenity goes below the value \code{tol}.
#'
#' @param nmin,ntot.min Additional stopping criteria. Respectively the
#'   minimal number of sites or station-year to be included in the region.
#'
#' @param nsim Number of simulations used to evaluate the heterogenous statistic
#'
#' @param distr.fix Logical, should the selection of the distribution be
#'   re-evaluated after removing the site.
#'
#' @param verbose Logical. Should a trace of the removed sites be display.
#'
#' @export
#'
#' @examples
#'
#' data(flowAtlantic)
#' 
#' ## Organize data
#' ams <- flowAtlantic$ams
#' ams$year <- format(ams$date, '%Y')
#' xmat <- DataWide(ams ~ id + year, ams)
#' 
#' ## Compute distance
#' coord <- flowAtlantic$info[,c('lon','lat')]
#' rownames(coord) <- flowAtlantic$info$id
#' h <- GeoDist(coord)
#' 
#' ## Fit a index flood model inside a pooling group
#' xmat0 <- FindNearest(xmat, h[1, colnames(xmat)], 25)
#' fit <- FitRegLmom(xmat)
#'
#' ## Remove site until homogeneity is reached
#' fit.new <- PoolRemove(fit, tol = 2)
#' 
#' ## Get the data from the pooling group
#' xmat1 <- xmat[,sitenames(fit.new)] 
#' 
#' 
PoolRemove <-
  function(obj,
           method = 'H1',
           tol = 2,
           nmin = 15,
           ntot.min = 0,
           nsim = 1000,
           distr.fix = FALSE,
           verbose = TRUE){

  site.removed <- list()

  ## Select the criteria for evaluating the homogeneity
  r <- which(c('H1','H2','H3') == method)

  if(is.null(obj$stat)){
    dd <- lmomRFA::as.regdata(data.frame(rownames(obj$lmom),
                                         obj$nrec, obj$lmom), FALSE)

    tst <- lmomRFA::regtst(dd, nsim)

    obj$stat <- c(tst$H, tst$Z)

  }

  if (obj$stat[r] <= tol){
    warning('The pooling group is already sufficiently homogenous')
    return(obj)
  }

  repeat{

    ## verify that removing one site will no lead to a too small region
    if(nrow(obj$lmom) - 1 < nmin)
      break

    ## monitoring (if require)
    if (verbose){
      print(paste('n:', nrow(obj$lmom),
                  ', h:', round(obj$stat[r],2),
                  ', t:', sum(obj$nrec), sep = '') )
    }


    ## remove one site
    suppressWarnings(obj.new <- PoolRemove0(obj, method = method,
                                        nsim = nsim, distr.fix = distr.fix))

    ## verify stopping criteria
    ntot.new <- sum(obj.new$nrec)

    if(ntot.new < ntot.min){
      warning('The minimal number of station-years have been reached')
      break

    } else{
      ## iterate
      obj <- obj.new
    }

    ## If a sufficient heterogeous criterion has been reached
    if (obj$stat[r] <= tol)
      break

  }

  return(obj)

}

PoolRemove0 <- function(obj, method, nsim, distr.fix){

  ## Number of site
  nk <- nrow(obj$lmom)

  ## Which homogeneity statistic to use
  r <- which(method == c('H1','H2','H3'))

  ## -------------------------
  ##  Find the site to remove
  ## -------------------------

  ## Create a regdata object
  dd <- lmomRFA::as.regdata(cbind(1:nk, obj$nrec, obj$lmom), FALSE)

  ## Function for computing the homogeneity H without a specific site
  FunHmg <- function(ii) lmomRFA::regtst(dd[-ii,],nsim)

  ## find the sites that improve best H
  ## Note that the target is always the first site and must be kept
  tst.new <- lapply(1:nk, FunHmg)
  hmg.new <- sapply(tst.new, getElement, 'H')
  mid <- which.min(abs(hmg.new[r,-1])) + 1
  tst0 <- tst.new[[mid]]

  ## -------------------------------
  ## update the pooling group object
  ## -------------------------------

  obj$lmom <- obj$lmom[-mid,]
  obj$nrec <- obj$nrec[-mid]
  obj$stat <- c(tst0$H, tst0$Z)
  obj$discord <- tst0$D

  ## For amax, update the selected distribution
  if (!distr.fix & obj$type == 'amax')
    obj$distr <- c('glo','gev','gno','pe3')[which.min(abs(obj$stat[4:7]))]

  ## Estimate the new regional L-moments
  obj$rlmom <- lmomRFA::regavlmom(dd[-mid,])

  ## Refit the growth curve with maybe new distribution
  if (obj$type == 'amax'){
    v <- lmomco::vec2lmom(obj$rlmom, lscale = FALSE)
    obj$para <- lmomco::lmom2par(v, obj$distr)$para

  } else if (obj$type == 'pot'){
    k <- 1/obj$rlmom[2]-2
    obj$para <- c(1 + k, k)
  }

  return(obj)
}
