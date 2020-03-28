##########################################################################
#' Find automatically a best threshold
#' 
#' The function \code{Findthresh} can be used to identify the best
#' threshold according to a specific rules. 
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' 
#' @param x output form \link{SearchThresh}.
#' 
#' @param method Method to identify the threshold. Must be one of 
#' \code{sgn}, \code{sgn-max}, \code{sgn-ppy}, \code{max} or \code{ppy}.
#' 
#' @param tol.sgn Selection criteria associated with a given p-value of 
#' the Anderson-Darling test.
#' 
#' @param tol.ppy Selection criteria associated with a given average of 
#'   peaks per year.
#' 
#' @param qua Flood quantiles. Must be one of \code{'q2'}, \code{'q5'},
#'   \code{'q10'}, \code{'q20'}, \code{'q50'} or \code{'q100'}.
#' 
#' @param tol.qua Selection criteria associated with a flood quantile.
#' 
#' @param ppy Range of accepted averages of peaks per year.
#'
#' @details
#' 
#' The method \code{sgn} find the first threshold that has a p-value over a 
#' given value (\code{tol.sgn}) of the Anderson-Darling test, while \code{max} 
#' find the maximum p-value.
#' The \code{ppy} method find the threshold that has the nearest average of
#' peaks per year to \code{tol.ppy}.
#' The method \code{sgn-max} find the thresholds associated with both methods
#' and return the lowest one. 
#' The method \code{sgn-ppy} try first to find the \code{sgn} threshold.
#' If not threshold meets the required p-value, the \code{ppy} threshold is 
#' returned.
#' 
#' The criteria \code{ppy}, initially filter all the candidates thresholds that 
#' are not found in a given interval of average peaks per year. 
#' With the \code{sgn} method an additional condition associated to a flood 
#' quantile \code{tol.qua} can be specified. 
#' It imposes that the relative discrepancies between the candidate threshold 
#' and a reference is respected in addition to the criteria with the p-value.
#' This reference is the average of the flood quantiles of the 5 lowest 
#' candidate thresholds in the set of candidates.
#' Normally, lower thresholds should have reached stability and their choice can
#' be controlled by the argument \code{ppy}.
#'
#' The method \code{fdr} is a variation of the \code{sgn} method. 
#' It verifies that the p-value of a given threshold is above a critical value 
#' and also  that here after the GPA distribution is a proper distribution for 
#' all the thresholds using false discovery rate (FDR) at the same significance level.
#' 
#' @references 
#' 
#' Durocher, M., Zadeh, S. M., Burn, D. H., & Ashkar, F. (2018). Comparison of 
#'   automatic procedures for selecting flood peaks over threshold based on 
#'   goodness-of-fit tests. Hydrological Processes, 0(0).
#'   https://doi.org/10.1002/hyp.13223
#'   
#' Solari, S., Eguen, M., Polo, M. J., & Losada, M. A. (2017). Peaks Over 
#'   Threshold (POT): A methodology for automatic threshold estimation using 
#'   goodness of fit p-value. Water Resources Research, 53(4), 2833-2849. 
#'   https://doi.org/10.1002/2016WR019426
  
#'
#' @export
#'
#' @examples
#' 
#' ## Create list of candidate threshold
#' lstu <- which.floodPeaks(flow~date, flowStJohn, u =500, r = 14)
#' lstu <- sort(unique(flowStJohn[lstu,'flow']))
#' lstu <- lstu[seq(1,length(lstu)-30,2)]
#'
#' out <- SearchThresh(flow~date, flowStJohn, u = lstu, declust = 'wrc', r = 14)
#'
#' cid <- c('u','ppy','ad')
#' FindThresh(out, method = 'sgn', tol.sgn = 0.25)[,cid]
#' FindThresh(out, method = 'ppy', tol.ppy = 2)[,cid]
#' FindThresh(out, method = 'max')[,cid]
#' 
#' ## Make find the first threshold that have at-least p-value > 0.1
#' ## and relative descripencies between Q100 of less than 20%.
#' ## Otherwise the ppy threshold is chosen.
#' ## Note here that sgn is chosen because the flood quantile stabilizes 
#' ## quickly. 
#' FindThresh(out, method = 'sgn-ppy', tol.sgn = 0.25, tol.ppy = 1.5,
#'            ppy = c(1,3), qua = 'q10', tol.qua = 0.2)[,cid]
#' 
#' 
FindThresh <- 
  function(x, 
           method = 'sgn',
           tol.sgn = 0.25, 
           tol.ppy = 2,
           qua = 'q10', 
           tol.qua = Inf,
           ppy = c(0, Inf)){

  ## Filter by PPY
  pid <- x$ppy >= ppy[1] & x$ppy <= ppy[2]

  if(any(pid)){
    x <- x[pid,]
  } else{
    stop('There is no threshold found in the required PPY range.')
  }

  ## Find automatic thresholds
  if(!(qua %in% c('q2','q5','q10','q20','q50','q100')))
    stop('The selected quantile (qua) is not valid')

  ## Relative discrepencies with lowest estimated
  qnmin <- min(5, sum(pid))
  qref <- mean(x[nrow(x)-1:qnmin,qua])
  quadif <- abs(1-x[,qua]/qref)

  ## Identify automatic thresholds
  idf <- which(x$ad >= tol.sgn & x$fdr >= tol.sgn)[1]
  ids <- which(x$ad >= tol.sgn & quadif <= tol.qua)[1]
  idx <- which.max(x$ad)
  idp <- which.min(abs(x$ppy - tol.ppy))

  rname <- method

  if(method == 'max'){
    id <- idx

  } else if(method == 'ppy'){
    id <- idp

  } else if(method == 'sgn' ){

    if(!is.na(ids)){
      id <- ids
    } else {
      id <- nrow(x)
    }
    
  } else if(method == 'fdr'){
    id <- idf 

  } else if(method == 'sgn-max'){
    id <- min(idx, ids)

  } else if(method == 'sgn-ppy'){

    if(!is.na(ids)){
      id <- ids
    } else{
      id <- idp
    }

  } else if(method == 'all'){
    id <- c(ids,idf,idx,idp)
    
    rname <- c('sgn','fdr','max','ppy')
    
  } else {
    stop('Not a valid automatic selection method.')
  }
  
  ans <- x[id,, drop = FALSE]
  
  rownames(ans) <- rname
  
  return(ans)
}