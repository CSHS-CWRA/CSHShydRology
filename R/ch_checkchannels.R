#' Check Channels
#' 
#' Generates a map of the generated channel layer.
#' 
#' This function generates a simple map of the drainage network plotted over the contours to allow a visual assessment.
#' 
#' @param dem Raster. DEM catchment was generated from
#' @param channels sf. channel polyline
#' @param outlet sf. location of catchment outlet
#' @return 
#' \item{check_map}{generates map with channel layer}
#' 
#' @author Dan Moore <dan.moore@ubc.ca>
#' @seealso \code{\link{ch_saga_fillsinks}} to fill sinks instead of removing
#' 
#' 
#' @examples 
#' \dontrun{
#' map <-  ch_checkchannels(dem, channels, outlet)
#' }
#' 
#' @importFrom sf st_bbox
#' @importFrom ggplot2 ggplot geom_sf coord_sf theme_bw 
#' @importFrom ggspatial annotation_north_arrow annotation_scale north_arrow_fancy_orienteering
#' @export
ch_checkchannels <- function(dem, channels, outlet) {
  
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
