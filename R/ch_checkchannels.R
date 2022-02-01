#' Check Channels
#' 
#' @description
#' Generates a map of the generated channel network layer. 
#' 
#' @details
#' Generates a simple map of the drainage network plotted over the contours to allow a visual assessment.
#' 
#' @param dem raster DEM that catchments were generated from
#' @param channels channel polyline (or channels list from \code{ch_wbt_channels}) (sf object)
#' @param outlet location of catchment outlet (sf object)
#' @return 
#' \item{check_map}{generates map with channel layer}
#' 
#' @author Dan Moore
#' @seealso \code{\link{ch_wbt_fillsinks}} to fill sinks instead of removing
#' 
#' @examples
#' 
#' \donttest{
#' # note: example not tested in package compilation

#' # create wd using base::tempdir()
#' wd <- tempdir()
#'
#' # download 25m DEM
#' ff <- "gs_dem25.tif"
#' ra_fn <- file.path(wd, ff)
#' ra_url <- sprintf("https://zenodo.org/record/4781469/files/%s",ff)
#' dem <- ch_get_url_data(ra_url, ra_fn)
#' 
#' # fill sinks
#' filled_dem <-  ch_wbt_removesinks(dem_raw=dem, saga_wd=saga_wd)
#' 
#' # determine contributing area raster using filled_dem
#' carea <- ch_saga_carea(filled_dem, saga_wd)
#' 
#' # generate channels sf object
#' channels <- ch_wbt_channels(dem=filled_dem, saga_wd=saga_wd, carea=carea)
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
  
  # check inputs
  if (missing(dem)) {
    stop("ch_checkchannels requires a raster dem to plot")
  }
  if (missing(channels)) {
    stop("ch_checkchannels requires sf channels polyline to plot")
  }
  if (missing(outlet)) {
    stop("ch_checkchannels requires an sf outlet to plot")
  }
  
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
