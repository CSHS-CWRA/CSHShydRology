#' Check Catchments
#'
#' Generates a map with contours, catchment polygons, and outlet
#'
#' @param dem Raster. DEM that catchment was generated from
#' @param catchment sf. catchment polygon
#' @param outlet sf. location of catchment outlet
#' @param outlet_label string. Label for outlet
#' @return Generates contour map with catchment polygon and outlet
#' @import sf raster ggplot2 ggspatial dplyr
#' @examples /dontrun{map <- ch_checkcatchment(dem, catchment_shape, outlet)}
ch_checkcatchment <- function(dem, catchment, outlet, outlet_label = NULL) {

  # create contours and get bounding box to set map limits
  contours <- ch_contours(dem)
  bb <- st_bbox(contours)
  # generate map
  check_map <- ggplot(data = contours) +
    geom_sf(data = contours, color = "grey") +
    geom_sf(data = outlet, pch = 21, bg = "blue") +
    geom_sf(data = st_geometry(catchment), fill = NA, color = "red") +
    annotation_north_arrow(style = north_arrow_fancy_orienteering,
                                      location = "tr",
                                      pad_x = unit(4, "mm"),
                                      pad_y = unit(6.5, "mm")) +
    annotation_scale() +
    coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4])) +
    theme_bw()
  
  print(check_map)
  nc <- length(catchment)
  
  # print catchment area with units
  if (is.null(outlet_label)) {
    labels <- as.character(1:nc)
  } else {
    labels <- outlet_label
  }
  
  area <- sf::st_area(catchment)
  units <- rep(paste0(attr(area, "units")$numerator[1], "^2"), length(area))
  value <- round(as.numeric(area))
  area_df <- outlet %>%
    mutate(label = labels, area = value, units = units)
  
  return(area_df)
} # end function ch_checkcatchment

