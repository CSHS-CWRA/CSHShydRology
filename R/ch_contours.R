#' Create Contours
#'
#' creates contour lines from a DEM
#'
#' @param dem Raster object of your dem in the desired projection - should have had sinks removed
#' @param zmin minimum elevation value for contours
#' @param zmax maximum elevation value for contours
#' @param n_levels number of contour lines
#' @param z_levels levels at which to plot contours
#' @return contours_sf  sf object containing contours
#' ch_contours()

ch_contours <- function(dem,
                        zmin = NULL, zmax = NULL,
                        n_levels = 10,
                        z_levels = NULL) {

  library(raster)
  library(sf)
  # determine contour levels
  if (is.null(z_levels)) {
    z = raster::getValues(dem)
    if (is.null(zmin)) zmin <- min(z, na.rm = TRUE)
    if (is.null(zmax)) zmax <- max(z, na.rm = TRUE)
    z_levels <- seq(zmin, zmax, length.out = n_levels)
  }
  # if dem includes sea level, start contours at 0.1 m to mimic coastline
  if (z_levels[1] <= 0) z_levels[1] <- 0.1
  # generate contours as a sf object
  contours_sf <- raster::rasterToContour(dem, levels = z_levels) %>%
    sf::st_as_sf()
  st_crs(contours_sf) <- crs(dem)
  return(contours_sf)
} # end function ch_contours()

