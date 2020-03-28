###############################################################################
#' Great-circle distance
#'
#' Returns the distance matrix of the great-circle distance between all pairs
#'   of sites.
#'   
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x  Dateset of coordinates (lon,lat).
#'
#' @param form Formula to identify coordinates in a dataset. 
#'   Must be of the form : \code{lon~lat}.
#'   
#' @param earth.radius Radius of the earth. Default 6371 km.
#' 
#' @param ... Other parameters.
#'
#' @export
#'
#' @examples
#'
#' attach(flowAtlantic)
#'
#' GeoDist(lon~lat, flowAtlantic$info)[1:5,1:5]
#' 
#'
GeoDist <- function(x, ...) UseMethod('GeoDist', x)

#' @export
#' @rdname GeoDist
GeoDist.default <- function(x, earth.radius = 6371, ...)
  fields::rdist.earth(as.matrix(x), miles = FALSE, R = earth.radius)

#' @export
#' @rdname GeoDist
GeoDist.formula <- function(form, x, ...)
  GeoDist(model.frame(form,x), ...)
