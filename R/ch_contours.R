#' Create Contours
#'
#' @description 
#' Creates contour lines from a DEM.
#' 
#' @details
#' Generates contour lines from a DEM, which are returned as a \pkg{terra} \code{SpatVector} object. 
#' The user can either provide a vector of elevation values by specifying the \code{z_levels} argument, 
#' or by supplying the minimum and maximum elevations (\code{zmin} and \code{zmax}) 
#' and the number of contour lines (\code{n_levels}).
#'
#' @param dem Raster object of your dem in the desired projection (note: should have had sinks removed).
#' @param zmin Minimum elevation value for contours. If not specified, minimum value \option{dem} is used.
#' @param zmax Maximum elevation value for contours. If not specified, maximum value \option{dem} is used.
#' @param n_levels Number of contour lines. Default is 10.
#' @param z_levels Levels at which to plot contours. If specified, overrides \option{zmin}, \option{zmax} and
#' \option{n_levels}.
#' @return  
#' \item{contours_sv}{SpatVector object containing contours}
#' 
#' @author Dan Moore
#' 
#' @examples
#' # use volcano DEM
#' dem <- ch_volcano_raster()
#' # generate contours
#' contours <- ch_contours(dem)
#' 
#' # plot contours map
#' terra::plot(contours)
#' 
#' @importFrom terra rast values as.contour crs
#' @export
ch_contours <- function(dem,
                        zmin = NULL, zmax = NULL,
                        n_levels = 10,
                        z_levels = NULL) {
  
  # check inputs
  if (missing(dem)) {
    stop("ch_contours requires a raster dem")
  }
  
  # determine contour levels
  if (is.null(z_levels)) {
    z <- values(dem)
    if (is.null(zmin)) zmin <- min(z, na.rm = TRUE)
    if (is.null(zmax)) zmax <- max(z, na.rm = TRUE)
    z_levels <- seq(zmin, zmax, length.out = n_levels)
  }
  # if dem includes sea level, start contours at 0.1 m to mimic coastline
  if (z_levels[1] <= 0) {z_levels[1] <- 0.1}
  # generate contours as a terra SpatVector object
  contours_sv <- terra::as.contour(dem, levels = z_levels)
  terra::crs(contours_sv) <- terra::crs(dem)
  return(contours_sv)
}
