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
#' \item{check_map}{a \pkg{ggplot} object of a map with channel layer}
#' 
#' @author Dan Moore
#' @seealso \code{\link{ch_checkcatchment}}  
#' @importFrom sf st_bbox st_geometry
#' @importFrom ggplot2 ggplot geom_sf coord_sf theme_bw 
#' @importFrom ggspatial annotation_north_arrow annotation_scale north_arrow_fancy_orienteering
#' @export
#' @examples 
#' library(raster)
#' test_raster <- ch_volcano_raster()
#' dem_raster_file <- tempfile(fileext = c(".tif"))
#' no_sink_raster_file <- tempfile("no_sinks", fileext = c(".tif"))
#' # write test raster to file
#' writeRaster(test_raster, dem_raster_file, format = "GTiff")
#' # remove sinks
#' removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, method = "fill")
#' # get flow accumulations
#' flow_acc_file <- tempfile("flow_acc", fileext = c(".tif"))
#' flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)
#' # get flow directions
#' flow_dir_file <- tempfile("flow_dir", fileext = c(".tif"))
#' flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
#' channel_raster_file <- tempfile("channels", fileext = c(".tif"))
#' channel_vector_file <- tempfile("channels", fileext = c(".shp"))
#' channels <- ch_wbt_channels(flow_acc_file, flow_dir_file, channel_raster_file,
#' channel_vector_file, 1)
#' # get pour points
#' pourpoint_file <- tempfile("volcano_pourpoints", fileext = ".shp")
#' pourpoints <- ch_volcano_pourpoints(pourpoint_file)
#' snapped_pourpoint_file <- tempfile("snapped_pourpoints", fileext = ".shp")
#' snapped_pourpoints <- ch_wbt_pourpoints(pourpoints, flow_acc_file, pourpoint_file,
#' snapped_pourpoint_file, snap_dist = 10)
#' check_map <- ch_checkchannels(test_raster, channels, snapped_pourpoints)
#' check_map
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
