#' Generate stream network
#'
#' @param fn_flowacc File name for flow accumulation grid.
#' @param fn_flowdir File name for flow direction grid.
#' @param fn_channel_ras File name for raster version of channel network.
#' @param fn_channel_vec File name for vector version of channel networks.
#' @param threshold Threshold for channel initiation.
#' @param ... Other parameters for \pkg{whitebox} function \code{wbt_extract_streams}
#' @author Dan Moore
#' @importFrom raster raster
#' @importFrom whitebox wbt_extract_streams wbt_raster_streams_to_vector
#' @importFrom sf st_crs write_sf
#' @importFrom stats step
#' @return Returns a \pkg{sf} vector object of the stream channels.
#' @export
#'
#' @examples 
#' # Only proceed if Whitebox executable is installed
#' library(whitebox)
#' if (check_whitebox_binary()){
#'   library(raster)
#'   test_raster <- ch_volcano_raster()
#'   dem_raster_file <- tempfile(fileext = c(".tif"))
#'   no_sink_raster_file <- tempfile("no_sinks", fileext = c(".tif"))
#' 
#'   # write test raster to file
#'   writeRaster(test_raster, dem_raster_file, format = "GTiff")
#' 
#'   # remove sinks
#'   removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, method = "fill")
#' 
#'   # get flow accumulations
#'   flow_acc_file <- tempfile("flow_acc", fileext = c(".tif"))
#'   flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)
#' 
#'   # get flow directions
#'   flow_dir_file <- tempfile("flow_dir", fileext = c(".tif"))
#'   flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
#'   channel_raster_file <- tempfile("channels", fileext = c(".tif"))
#'   channel_vector_file <- tempfile("channels", fileext = c(".shp"))
#'   channels <- ch_wbt_channels(flow_acc_file, flow_dir_file, channel_raster_file,
#'   channel_vector_file, 1)
#'   plot(channels)
#' } else {
#'   message("Examples not run as Whitebox executable not found")
#' }

ch_wbt_channels <- function(fn_flowacc, fn_flowdir,
                            fn_channel_ras, fn_channel_vec, 
                            threshold = NULL, ...) {
  ch_wbt_check_whitebox()
  if (!file.exists(fn_flowacc)) {
    stop("Error: input flow accumulation file does not exist")
  }
  
  if (!file.exists(fn_flowdir)) {
    stop("Error: input flow direction file does not exist")
  }
  if (is.null(threshold)) {
    step("Error: threshold for channel initiation not specified")
  }
  
  message("ch_wbt: Generating stream network")
  wbt_extract_streams(fn_flowacc, fn_channel_ras, threshold = threshold, ...)
  wbt_raster_streams_to_vector(fn_channel_ras, fn_flowdir, fn_channel_vec)
  channel_vec <- st_read(fn_channel_vec)
  if(is.na(st_crs(channel_vec))) {
    sf::st_crs(channel_vec) <- st_crs(raster(fn_channel_ras))
    write_sf(channel_vec, fn_channel_vec)
  } 
  return(channel_vec)
}
