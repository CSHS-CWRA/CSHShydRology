#' Check Channels
#' 
#' Generates a map of the generated channel layer
#' 
#' @param dem Raster. DEM catchment was generated from
#' @param catchment sf. catchment polygon
#' @param outlet sf. location of catchment outlet
#' @return generates map with channel layer
#' @import sf raster ggplot2 ggspatial
#' @examples \dontrun{map <-  ch_checkchannels(dem, channels, outlet)}

ch_checkchannels <- function(dem, channels, outlet) {
  contours <- ch_contours(dem)
  # get bounding box for contours to set map limits
  bb <- st_bbox(contours)
  # generate map
  check_map <- ggplot2::ggplot(data = contours) +
    geom_sf(data = contours, color = "grey") +
    geom_sf(data = outlet, pch = 21, bg = "black") +
    geom_sf(data = sf::st_geometry(channels), color = "blue") +
    ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering, 
                                      location = "tr",
                                      pad_x = unit(4, "mm"), 
                                      pad_y = unit(6.5, "mm")) +
    ggspatial::annotation_scale() +
    coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4])) +
    theme_bw()
  return(check_map)
} # end function ch_checkchannels

