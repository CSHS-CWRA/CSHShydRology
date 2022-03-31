###########################################################################
#' Extracting peaks
#'
#' Returns the indices of the peaks above a threshold according to the
#' declustering method put in place by the Water Resources Council or simple
#' run declustering.
#' See Lang et al. (1999) for more details.
#'
#' @param x,form If numeric, \code{x} is a vector of data. If a formula is passed
#'
#'
#' @param dt Date or time of observations. If not provided, regular step
#'   are asumed.
#'   
#' @param u Threshold.
#'
#' @param r,rlow,ini Declustering parameters. See details.
#' 
#' @param ... Other parameters.
#'
#' @details
#'
#' Two conditions are required for peaks to not be
#' rejected. First, two peaks \code{Q1} and \code{Q2} must be separated by a
#'  period of at least \code{r} days, where one recommendation is
#'   \deqn{4 days + log(A)}
#' and \eqn{A} is the drainage area in squared kilometers.
#' The second conditions is
#' \deqn{Xmin > rlow * min(Q1,Q2).} where \code{Xmin} is lowest point
#' between \code{Q1} and \code{Q2}. By defautlt, \code{rlow = 0.75}.
#' When one of the two conditions is not statisfied the lowest of the two
#' peaks is discarded.
#' The 2 conditions are verify sequentially, from an initial set of peaks.
#' If \code{ini = 'run'}, a run declustering method with one lag is first used to
#' filter the initial set of peaks to the maximums of the respective clusters.
#' If \code{ini = 'lmax'} the 2 condition are verified accross all local maximums.
#' If \code{ini = 'wrc'} (default), the second condition id verified
#' next the first condition is verify on the previously extracted peaks.
#' The two version are very similar and differ only on few cases where the
#' modified version is more conservative and reject peaks that are kept
#' in the initial version.
#'
#' The function \code{which.clusters} is returning the indices of the peaks
#' identified by the run declustering method where clusters are separated by a
#' period of \code{r} consecutive values under the threshold.
#'
#' @section References:
#'
#' Lang M, Ouarda TBMJ, Bobee B. (1999) Towards operational guidelines for
#'   over-threshold modeling. Journal of Hydrology. Dec 6;225(3):103-17.
#'
#' @export
#'
#' @examples
#'
#' # Declustering using the flood recommendation.
#' cid <- which.floodPeaks(flow~date, flowStJohn, u = 1000, r = 14, rlow = .75,
#'                          ini = 'wrc')
#'
#' plot(flowStJohn, type = 'l')
#' points(flowStJohn[cid,], col = 'red', pch = 16)
#'
#' ## Simpler run declustering
#' cid <- which.clusters(flowStJohn$flow, u = 1000, r = 14)
#'
#' plot(flowStJohn, type = 'l')
#' points(flowStJohn[cid,], col = 'red', pch = 16)
which.floodPeaks <- function(x,...) UseMethod('which.floodPeaks', x)

#' @export
#' @rdname which.floodPeaks
which.floodPeaks.numeric <- 
  function(x, 
           dt = NULL, 
           u,
           r = 1, 
           rlow = 0.75, 
           ini = 'wrc', 
           ...){

  ## If time is not provided
  if(is.null(dt))
    dt <- seq_along(x)

  ## Verify that the time vector are matching
  if(length(x) != length(dt))
    stop('Length of the Time vector do not match!')

  ## Extract all peaks above threshold
  if(ini == 'clust'){
    peakId <- which.clusters(x, u, r = 1, dt = dt)

  } else if(ini == 'lmax'){
    isPeak <- rep(FALSE, length(x))
    isPeak[which.lmax(x)] <- TRUE
    isPeak[x <= u] <- FALSE
    peakId <- which(isPeak)

  } else if(ini == 'wrc'){
    peakId <- which.floodPeaks(x, dt, u, r = 0, rlow, ini = 'lmax')

  } else if(ini == 'all'){
    peakId <- seq_along(x)
  }

  ## Case there is just one peak above threshold
  if(length(peakId) < 2) return(peakId)

  ## Loop accross and keep only
  myId <- peakId[1]
  ans <- list()

  for(nextId in peakId[-1]){

    ## Verify if lag r is sufficient
    if(abs(dt[nextId]- dt[myId]) < r){

      ## If not, Keep the highest peak
      if(x[nextId] > x[myId])
        myId <- nextId

      next
    }

    ## Verify that the a minimal intermediate point is reached
    if(min(x[seq(myId, nextId)]) > rlow * min(x[myId], x[nextId])){

      ## If not, Keep the highest peak
      if(x[nextId] > x[myId])
        myId <- nextId

      next
    }

    ## Otherwise save the identified peak id
    ans <- append(ans,myId)

    ## And move to the next one
    myId <- nextId

  }#endfor

  ## Add the last peaks
  ans <- append(ans,myId)

  return(unlist(ans))
}


#' @export
#' @rdname which.floodPeaks
which.floodPeaks.formula <- function(form, x, u, ...){
  xdf <- model.frame(form, x)
  which.floodPeaks.numeric(xdf[,1], dt = xdf[,2], u, ...)
}

#' @export
#' @rdname which.floodPeaks
which.floodPeaks.data.frame <- function(x, u, ...){
  which.floodPeaks.numeric(x[,1], dt = x[,2], u, ...)
}


#' @export
#' @rdname which.floodPeaks
which.clusters <- function(x,...) UseMethod('which.clusters',x)

#' @export
#' @rdname which.floodPeaks
which.clusters.formula <- function(form, x, u, r = 1, ...){
   x <- model.frame(form,x)
   which.clusters(x[,1], x[,2], u = u, r= r)
}

#' @export
#' @rdname which.floodPeaks
which.clusters.numeric <- function(x, dt = NULL, u, r = 1, ...){
  z <- seq_along(x)

  if(is.null(dt))
    dt <- z

  if(length(dt) != length(x))
    stop('Different length between data and time.')

  ## Keep value above threshold values, remove missing
  ## Also make sure that time value are finite
  id <- x > u & !is.na(x) & is.finite(dt)
  x  <- x[id]
  z  <- z[id]
  dt <- dt[id]

  ## Split data into cluster of adjacent events
  id <- cumsum(c(1,diff(dt)) > r)
  x  <- split(x,id)
  z  <- split(z,id)

  ## Find the location of the maximum in each cluster and return it
  id <- lapply(x, which.max)
  return(as.numeric(mapply('[',z,id)))
}


# Function that return the Local maximums
which.lmax <- function(x){
  xlow <- c(-Inf, x[1:(length(x)-1)])
  xup <- c(x[2:length(x)],-Inf)
  return(which(x > xlow & x >= xup))
}
