##############################################################################
#' Distance in seasonal space
#'
#' Return a matrix of distances between points in the seasonal space that
#' characterizes timing and regularity.
#' It is equivalent to Euclidean distance applied to regularity (radius)
#' and timing (angle) separately.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param r,a Coordinates in the seasonal space: radius `r` and angle `a`.
#'
#' @param form,x Formula and dataset providing the coordinates of the
#'   seasonal space. Must be of the form `radius ~ angle`.
#'
#' @param w Weight to favor angle over radius. 
#'   By default it is 1/pi, which bring angle in the interval [0,1]. 
#'
#' @references
#'
#' Durocher, M., Burn, D. H., & Ashkar, F. (2019). Comparison of estimation
#'   methods for a nonstationary index-flood model in flood frequency
#'   analysis using peaks over threshold. https://doi.org/10.31223/osf.io/rnepc
#'
#' @export
#'
#' @examples
#'
#' scoord <- data.frame(radius = runif(5), 
#'                      angle = runif(5,0,2*pi))
#' 
#' DistSeason(scoord)
#' DistSeason(radius ~ angle , scoord)
#'
#'
DistSeason <- function(x,...)  UseMethod('DistSeason', x)

#' @export
DistSeason.numeric <- function(r, a, w = 1/pi){

  ## Extract the pairs or every angles
  n <- length(a)

  if(length(r) != n)
    stop('Coordinates must be of the same length')

  id <- expand.grid(1:n, 1:n)
  aii <- a[id[,1]]
  ajj <- a[id[,2]]

  ## Compute the standardized absolute differences between angles
  mn <- pmin(aii,ajj)
  d <- pmax(aii-mn, ajj-mn)
  d <- pmin(2*pi-d, d)*w
  a.mat <- matrix(d, nrow = n)

  ## Compute the absolute differences between radius
  r.mat <- as.matrix(dist(r, method = 'man'))

  ## squared distances
  return(sqrt(r.mat^2 + a.mat^2))
}

#' @export
DistSeason.matrix <- function(x, w = 1/pi)
  DistSeason(x[,1], x[,2], w)

#' @export
#' @rdname DistSeason
DistSeason.data.frame <- function(x, w = 1/pi)
  DistSeason(x[,1], x[,2], w)

#' @export
#' @rdname DistSeason
DistSeason.formula <- function(form, x, w = 1/pi){
  x <- as.data.frame(x)
  x <- model.frame(form, x) ## form = r ~ a
  
  return(DistSeason(x[,1], x[,2], w))
}