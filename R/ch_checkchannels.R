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
#' @param main_label Main label for channel plot.
#' @param channel_colour Colour for channel. Default is "blue".
#' @param pp_colour Colour for catchment pour points. Default is "red".
#' @param contour_colour Colour for contours Default is "grey".
#' @param outlet location of catchment outlet (sf object)
#'
#' @return 
#' \item{check_map}{a \pkg{ggplot} object of a map with channel layer}
#' 
#' @author Dan Moore
#' @seealso \code{\link{ch_checkcatchment}}  
#' @importFrom sf st_bbox st_geometry
#' @importFrom ggplot2 ggplot geom_sf coord_sf theme_bw 
#' @importFrom ggspatial annotation_north_arrow annotation_scale north_arrow_fancy_orienteering
#' @export
#' @examples 
#' \donttest{
#' # Not tested automatically as requires installation of Whitebox
#' library(raster)
#' test_raster <- ch_volcano_raster()
#' dem_raster_file <- tempfile(fileext = c(".tif"))
#' no_sink_raster_file <- tempfile("no_sinks", fileext = c(".tif"))
#' 
#' # write test raster to file
#' writeRaster(test_raster, dem_raster_file, format = "GTiff")
#' 
#' # remove sinks
#' removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, method = "fill")
#' 
#' # get flow accumulations
#' flow_acc_file <- tempfile("flow_acc", fileext = c(".tif"))
#' flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)
#' 
#' # get flow directions
#' flow_dir_file <- tempfile("flow_dir", fileext = c(".tif"))
#' flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
#' channel_raster_file <- tempfile("channels", fileext = c(".tif"))
#' channel_vector_file <- tempfile("channels", fileext = c(".shp"))
#' channels <- ch_wbt_channels(flow_acc_file, flow_dir_file, channel_raster_file,
#' channel_vector_file, 1)
#' 
#' # get pour points
#' pourpoint_file <- tempfile("volcano_pourpoints", fileext = ".shp")
#' pourpoints <- ch_volcano_pourpoints(pourpoint_file)
#' snapped_pourpoint_file <- tempfile("snapped_pourpoints", fileext = ".shp")
#' snapped_pourpoints <- ch_wbt_pourpoints(pourpoints, flow_acc_file, pourpoint_file,
#' snapped_pourpoint_file, snap_dist = 10)
#' ch_checkchannels(test_raster, channels, snapped_pourpoints)
#' }
ch_checkchannels <- function(dem, channels, outlet = NULL, main_label = "",
                             channel_colour = "blue", pp_colour = "red",
                             contour_colour = "grey") {
  
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
  

  contours <- ch_contours(dem)
  # get bounding box for contours to set map limits
  bb <- sf::st_bbox(contours)
  # generate map
  check_map <- ggplot2::ggplot(data = contours) +
    geom_sf(data = contours, color = contour_colour) +
    geom_sf(data = sf::st_geometry(channels), color = channel_colour) +
    ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering, 
                                      location = "tr",
                                      pad_x = unit(4, "mm"), 
                                      pad_y = unit(6.5, "mm")) +
    ggspatial::annotation_scale() +
    coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), 
             datum = st_crs(channels)) +
    labs(title = main_label) +
    theme_bw()
  if (!is.null(outlet)) {
    check_map <- check_map +
      geom_sf(data = outlet, pch = 21, bg = pp_colour) +
      coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), 
               datum = st_crs(channels))
  }
  print(check_map)
  return(check_map)
} 
