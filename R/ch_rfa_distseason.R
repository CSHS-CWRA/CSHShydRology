#' Distance in seasonal space
#'
#' @description Calculates a matrix of distances between points in the seasonal 
#' space that characterizes timing and regularity.
#' It is equivalent to Euclidean distance applied to regularity (radius)
#' and timing (angle) separately.
#' 
#' @author Martin Durocher
#'
#' @param x,a Coordinates in the seasonal space. 
#'  Can be a data.frame or vectors with radius \code{x} and angle \code{a}.
#'
#' @param form Formula and dataset providing the coordinates of the
#'   seasonal space. Must be of the form \code{radius ~ angle}.
#'
#' @param w Weight to favor angle over radius. 
#'   By default it is 1/pi, which bring angle in the interval [0,1]. 
#'   
#' @param ... Other parameters.
#' 
#' @seealso \link{ch_rfa_seasonstat}
#'
#' @references
#'
#' Durocher, M., Burn, D. H., & Ashkar, F. (2019). Comparison of estimation
#'   methods for a nonstationary index-flood model in flood frequency
#'   analysis using peaks over threshold. https://doi.org/10.31223/osf.io/rnepc
#'
#' @export
#' 
#' @importFrom stats model.frame dist
#' 
#' @return Returns a matrix of distances between points in the seasonal 
#' space that characterizes timing and regularity.
#' @examples
#'
#' scoord <- data.frame(radius = runif(5), 
#'                      angle = runif(5,0,2*pi))
#' 
#' ch_rfa_distseason(radius ~ angle , scoord)
#'
#'
ch_rfa_distseason <- function(x, ...)  UseMethod('ch_rfa_distseason', x)

#' @export
#' @rdname ch_rfa_distseason
ch_rfa_distseason.numeric <- function(x, a, w = 1/pi, ...){

  ## Extract the pairs or every angles
  n <- length(a)

  if (length(x) != n)
    stop('Coordinates must be of the same length')

  id <- expand.grid(1:n, 1:n)
  aii <- a[id[,1]]
  ajj <- a[id[,2]]

  ## Compute the standardized absolute differences between angles
  mn <- pmin(aii,ajj)
  d <- pmax(aii - mn, ajj - mn)
  d <- pmin(2*pi - d, d)*w
  a.mat <- matrix(d, nrow = n)

  ## Compute the absolute differences between radius
  r.mat <- as.matrix(dist(x, method = 'man'))

  ## squared distances
  return(sqrt(r.mat^2 + a.mat^2))
}

#' @export
ch_rfa_distseason.matrix <- function(x, w = 1/pi, ...)
  ch_rfa_distseason(x[,1], x[,2], w)

#' @export
#' @rdname ch_rfa_distseason
ch_rfa_distseason.data.frame <- function(x, w = 1/pi, ...)
  ch_rfa_distseason(x[,1], x[,2], w)

#' @export
#' @rdname ch_rfa_distseason
ch_rfa_distseason.formula <- function(form, x, w = 1/pi, ...){
  x <- as.data.frame(x)
  x <- model.frame(form, x) ## form = r ~ a
  
  return(ch_rfa_distseason(x[,1], x[,2], w))
}
