###############################################################################
#' Fitting an index-flood model using regional L-moments
#'
#' Returns the at-site L-moments, regional L-moments and the parameters of
#' the regional growth curve for an index-flood model. 
#' If required, the homogeneity and goodness-of-fit measures are included.
#' The function can be used for analyzing both annual maximums and peaks over
#' threshold.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' 
#' @param x Dataset with sites in columns and time in rows.
#'
#' @param distr Regional distribution. See \link{Amax}.
#'   If \code{NULL} an automatic selection is performed.
#'
#' @param type Type of input data. Either annual maximums (\code{'amax'}) or
#'   peaks over threshold (\code{'pot'}).
#'   
#' @param nmom Number of L-moments to evaluate.
#'
#' @param diagnostic Should the homogeneity (H) and goodness of fit
#'   (Z-score) be evaluated.
#'
#' @param diagnostic.nsim Number of simulations used to evaluate the
#'   diagnostic statistics.
#' 
#' @param object Output from \code{FitRegLmom}.
#' 
#' @param ... Other parameters.
#'
#' @details 
#' 
#' The function \code{coef} return the parameter of the at-site distributions
#' estimated L-moments. The function \code{plot} will present a L-moment ratio
#' diagram.
#'
#' @return
#' \describe{
#'   \item{type}{Type of input data}
#'   \item{distr}{Regional distribution}
#'   \item{para}{Parameters of the regional distribution}
#'   \item{lmom}{At-site L-moments.}
#'   \item{rlmom}{Regional L-moments.}
#'   \item{nrec}{Record lengths.}
#'   \item{stat}{Homogenous criteria and Z-score for goodness-of-fit.}
#'   \item{discord}{Discordance measures.}
#' }
#' 
#' @seealso \link{predict.reglmom}, \link{PoolRemove}.
#'
#' @section References:
#'
#' Hosking, J. R. M., & Wallis, J. R. (1997). Regional frequency analysis:
#'   an approach based on L-moments. Cambridge Univ Pr.
#'
#' Mostofi Zadeh, S., Burn, D.H. (2019). A Super Region Approach to Improve
#'   Pooled Flood Frequency Analysis. Canadian Water Resources Journal
#'   0, 1-14. https://doi.org/10.1080/07011784.2018.1548946
#'
#' Mostofi Zadeh, S., Durocher, M., Burn, D.H., Ashkar, F., 2019.
#'   Pooled flood frequency analysis: a comparison based on peaks-over-threshold
#'   and annual maximum series. Hydrological Sciences Journal 0, null.
#'   https://doi.org/10.1080/02626667.2019.1577556
#'
#' @import lmomRFA
#' @export
#'
#' @examples
#'
#' data(flowAtlantic)
#' xd <- cbind(flowAtlantic$ams, 
#'              year = format(flowAtlantic$ams$date, '%Y'))
#'              
#' xmat <- DataWide(ams ~ id + year, xd)
#'
#' fit <- FitRegLmom(xmat)
#' print(fit)
#' plot(fit)
#' coef(fit, 'pe3')
#'
#'
FitRegLmom <-
  function(x,
           distr = NULL,
           type = 'amax',
           diagnostic = FALSE,
           diagnostic.nsim = 1000,
           nmom = 4){

  ## verify that x is a matrix
  if(is.data.frame(x)){
    x <- as.matrix(x)
  } else if(!is.matrix(x)){
    stop('Must be a matrix with sites in columns')
  }

  ## ------------------------------ ##
  ##  Evaluate the at-site L-moments
  ## ------------------------------ ##

  ## There should be at least 4 L-moments or 5 if it is a Wakaby distribution

  if(is.null(distr)){
    nmom <- max(4,nmom)
  } else if(distr == 'wak'){
    nmom <- max(5,nmom)
  } else {
    nmom <- max(4,nmom)
  }

  ## Compute the Lmoments
  lmom <- t(apply(x, 2, lmom::samlmu, nmom = nmom))

  lmom.name <- paste0('TAU',1:nmom)
  lmom.name[1:4] <- c('L1','LCV','LSK','LKUR')
  colnames(lmom) <- lmom.name

  lmom[,2] <- lmom[,2]/lmom[,1]

  # Create names if necessary
  if(is.null(rownames(lmom))){
    sname <- format(1:nrow(lmom), trim = FALSE, justify = 'left')
    rownames(lmom) <- paste0("site_", gsub(' ','0',sname))
  }

  ## ------------------------------ ##
  ##  Compute regional L-moments
  ## ------------------------------ ##

  ## record length
  nrec <- apply(x,2, function(z) sum(!is.na(z)))

  ## compute the regional lmoments
  dd <- as.regdata(data.frame(rownames(lmom), nrec, lmom), FALSE)
  rlmom <- regavlmom(dd)

  names(rlmom) <- lmom.name

  ##------------------------------------------##
  ## Homogenity test and choice of a distribution
  ##------------------------------------------##

  ## If no distribution is passed, the diagnostic routine
  ## must be performed on the selected growth curve
  if (is.null(distr))
    diagnostic <- TRUE

  ## If necessary evaluate the statistics H and Z
  if (diagnostic){

    tst <- regtst(dd, diagnostic.nsim)

    stat <- c(tst$H, tst$Z)
    names(stat) <- c('H1','H2','H3','Zglo','Zgev','Zgno','Zpe3','Zgpa')
    discord <- tst$D

    if (is.null(distr))
      distr <- c('glo','gev','gno','pe3')[which.min(abs(stat[4:7]))]

  } else {
    ## If no diagnostic is performed
    stat <- NULL
    discord <- NULL

  }

  ## ------------------------------------- ##
  ## Estimation of the regional parameter
  ## ------------------------------------- ##

  ## POT method always use Pareto distribution
  if (type == 'pot')
      distr <- 'gpa'

  if (type == 'amax'){
    v <- lmomco::vec2lmom(rlmom, lscale = FALSE)
    para <- lmomco::lmom2par(v, distr)$para

  } else if (type == 'pot'){
    k <- 1/rlmom[2]-2
    para <- c(0, 1 + k, k)
    names(para) <- c('xi','alpha','kappa')

  }

  ans <- list(type = type,
              distr = distr,
              para = para,
              lmom = lmom,
              rlmom = rlmom,
              nrec = nrec,
              stat = stat,
              discord = discord)

  class(ans) <- 'reglmom'

  return(ans)
}
