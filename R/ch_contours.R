#' Create Contours
#'
#' @description 
#' Creates contour lines from a DEM.
#' 
#' @details
#' Generates contour lines from a DEM, which are returned as an sf object. 
#' The user can either provide a vector of elevation values by specifying the \code{z_levels} argument, 
#' or by supplying the minimum and maximum elevations (\code{zmin} and \code{zmax}) 
#' and the number of contour lines (\code{n_levels}).
#'
#' @param dem raster object of your dem in the desired projection (note: should have had sinks removed)
#' @param zmin minimum elevation value for contours
#' @param zmax maximum elevation value for contours
#' @param n_levels number of contour lines
#' @param z_levels levels at which to plot contours
#' @return  
#' \item{contours_sf}{sf object containing contours}
#' 
#' @author Dan Moore
#' @seealso \code{\link{ch_saga_fillsinks}} to fill sinks instead of removing
#' 
#' @examples
#' 
#' \donttest{
#' # note: example not tested in package compilation
#' # - requires creating and accessing a temporary directory
#' # - requires downloading spatial data from Zenodo repository
#' # - requires multiple potentially lengthy GIS operations
#' 
#' # create saga wd using base::tempdir()
#' saga_wd <- tempdir()
#'
#' # download 25m DEM
#' ff <- "gs_dem25.tif"
#' ra_fn <- file.path(saga_wd, ff)
#' ra_url <- sprintf("https://zenodo.org/record/4781469/files/%s",ff)
#' dem <- ch_get_url_data(ra_url, ra_fn)
#' 
#' # generate contours
#' contours <- ch_contours(dem)
#' 
#' # plot contours map
#' plot(contours)
#' }
#' 
#' @importFrom raster raster getValues rasterToContour crs
#' @importFrom sf st_as_sf st_crs
#' 
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
    z = raster::getValues(dem)
    if (is.null(zmin)) zmin <- min(z, na.rm = TRUE)
    if (is.null(zmax)) zmax <- max(z, na.rm = TRUE)
    z_levels <- seq(zmin, zmax, length.out = n_levels)
  }
  # if dem includes sea level, start contours at 0.1 m to mimic coastline
  if (z_levels[1] <= 0) {z_levels[1] <- 0.1}
  # generate contours as a sf object
  contours_sf <- raster::rasterToContour(dem, levels = z_levels) %>%
    sf::st_as_sf()
  sf::st_crs(contours_sf) <- raster::crs(dem)
  return(contours_sf)
}
