#' Check Channels
#' 
#' Generates a map of the generated channel layer.
#' 
#' This function generates a simple map of the drainage network plotted over the contours to allow a visual assessment.
#' 
#' @param dem Raster. DEM catchment was generated from
#' @param channels sf. channel polyline (or channels list from \code{ch_saga_channels})
#' @param outlet sf. location of catchment outlet
#' @return 
#' \item{check_map}{generates map with channel layer}
#' 
#' @author Dan Moore <dan.moore@ubc.ca>
#' @seealso \code{\link{ch_saga_fillsinks}} to fill sinks instead of removing
#' 
#' 
#' @examples 
#' @examples
#' 
#' \dontrun{
#' # note: example not run in package compilation
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
#' # fill sinks
#' filled_dem <-  ch_saga_fillsinks(dem_raw=dem, saga_wd=saga_wd)
#' 
#' # determine contributing area raster using filled_dem
#' carea <- ch_saga_carea(filled_dem, saga_wd)
#' 
#' # generate channels sf object
#' channels <- ch_saga_channels(dem=filled_dem, saga_wd=saga_wd, carea=carea)
#' 
#' # download station locations (use as catchment outlets)
#' ff <- "gs_weirs.GeoJSON"
#' gs_fn <- file.path(saga_wd, ff)
#' gs_url <- sprintf("https://zenodo.org/record/4781469/files/%s",ff)
#' stns <- ch_get_url_data(gs_url, gs_fn)
#' 
#' # check channels
#' ch_checkchannels(dem=filled_dem, channels, outlet=stns)
#' }
#' 
#' @importFrom sf st_bbox st_geometry
#' @importFrom ggplot2 ggplot geom_sf coord_sf theme_bw 
#' @importFrom ggspatial annotation_north_arrow annotation_scale north_arrow_fancy_orienteering
#' @export
ch_checkchannels <- function(dem, channels, outlet) {
  
  # add handling for directly passing output from ch_saga_channels function
  if (class(channels) == "list" & "channels" %in% names(channels)) {
    channels <- channels$channels
  }
  
  contours <- ch_contours(dem)
  # get bounding box for contours to set map limits
  bb <- st_bbox(contours)
  # generate map
  check_map <- ggplot(data = contours) +
    geom_sf(data = contours, color = "grey") +
    geom_sf(data = outlet, pch = 21, bg = "black") +
    geom_sf(data = sf::st_geometry(channels), color = "blue") +
    annotation_north_arrow(style = north_arrow_fancy_orienteering, 
                                      location = "tr",
                                      pad_x = unit(4, "mm"), 
                                      pad_y = unit(6.5, "mm")) +
    annotation_scale() +
    coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4])) +
    theme_bw()
  return(check_map)
}
