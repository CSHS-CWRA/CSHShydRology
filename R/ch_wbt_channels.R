#' Generate stream network
#'
#' @param fn_flowacc 
#' @param fn_flowdir 
#' @param fn_channel_ras 
#' @param fn_channel_vec 
#' @param threshold 
#' @param ... 
#' @author Dan Moore
#' @importFrom raster raster
#' @importFrom whitebox wbt_extract_streams wbt_raster_streams_to_vector
#' @importFrom sf st_crs write_sf
#' @return
#' @export
#'
#' @examples
ch_wbt_channels <- function(fn_flowacc, fn_flowdir,
                            fn_channel_ras, fn_channel_vec, 
                            threshold = NULL, ...) {
  if (!file.exists(fn_flowacc)) {
    stop("Error: input flow accumulation file does not exist")
  }
  if (is.null(threshold)) {
    step("Error: threshold for channel initiation not specified")
  }
  
  message("ch_wbt: Generating stream network")
  
  wbt_extract_streams(fn_flowacc, fn_channel_ras, threshold = threshold, ...)
  wbt_raster_streams_to_vector(fn_channel_ras, fn_flowdir, fn_channel_vec)
  channel_vec <- st_read(fn_channel_vec)
  if(is.na(st_crs(channel_vec))) {
    st_crs(channel_vec) <- st_crs(raster(fn_channel_ras))
    write_sf(channel_vec, fn_channel_vec)
  } 
  return(channel_vec)
}
