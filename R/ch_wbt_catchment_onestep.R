#' Delineates a catchment in a single step
#' 
#' @description Calls all of the \code{ch_wbt} and other functions required to do the sub-tasks
#' required to delineate a catchment. The names of files to be created
#' are taken from the list created by the function \code{ch_wbt_filenames}.
#'
#' @param wd Name of working directory.
#' @param in_dem  File name for original DEM. 
#' @param pp_sf Vector containing pour points.
#' @param sink_method Method for sink removal as used by \code{ch_wbt_removesinks}.
#' @param dist Maximum search distance for breach paths in cells. Required if \code{sink_method = "breach_leastcost"}.
#' @param check_catchment If \code{TRUE} (the default) \code{ch_checkcatchment} will be called
#' after the catchment is created.
#' @param threshold Threshold for channel initiation.
#' @param snap_dist Maximum pour point snap distance in map units. 
#' @param cb_colour Colour for catchment outline. Default is "red".
#' @param pp_colour Colour for catchment pour points. Default is "red".
#' @param channel_colour Colour for channel. Default is "blue".
#' @param contour_colour Colour for contours Default is "grey".
#' @param plot_na If \code{TRUE} (the default) a north arrow is added to the plot.
#' @param plot_scale If \code{TRUE} (the default) a scale bar is added to the plot.
#' @param na_location Location for the north arrow. Default is \option{tr}, i.e. top-right.
#' @param scale_location Location for the scale bar. Default is \option{bl}, i.e. bottom-left.
#' @param ... Extra parameters for \code{ch_wbt_removesinks}.
#' @author Dan Moore and Kevin Shook
#' @seealso \code{\link{ch_wbt_filenames}}
#' @importFrom raster raster
#' @importFrom whitebox wbt_extract_streams wbt_raster_streams_to_vector wbt_snap_pour_points wbt_watershed wbt_raster_to_vector_polygons
#' @importFrom sf st_crs write_sf st_write
#' @importFrom magrittr %>%
#' @return Returns an \pkg{sp} object of the delineated catchment.
#' @export
#'
#' @examples
#' # Only proceed if Whitebox executable is installed
#' library(whitebox)
#' if (check_whitebox_binary()){
#'   library(raster)
#'   test_raster <- ch_volcano_raster()
#'   dem_raster_file <- tempfile(fileext = c(".tif"))
#'   # write test raster to file
#'   writeRaster(test_raster, dem_raster_file, format = "GTiff")
#'   wd <- tempdir()
#'   pourpoint_file <- tempfile("volcano_pourpoints", fileext = ".shp")
#'   pourpoints <- ch_volcano_pourpoints(pourpoint_file)
#'   catchment <- ch_wbt_catchment_onestep(wd = wd, in_dem = dem_raster_file, 
#'   pp_sf = pourpoints, sink_method = "fill", threshold = 1, snap_dist = 10)
#' } else {
#'   message("Examples not run as Whitebox executable not found")
#' }
ch_wbt_catchment_onestep <- function(wd, in_dem, pp_sf, 
                                     sink_method = "breach_leastcost", dist = NULL, 
                                     check_catchment = TRUE, threshold = NULL, snap_dist = NULL, 
                                     cb_colour = "red", pp_colour = "red",
                                     channel_colour = "blue", contour_colour = "grey",       
                                     plot_na = TRUE, plot_scale = TRUE,
                                     na_location = "tr", scale_location = "bl", ...) {
  ch_wbt_check_whitebox()
  
  if (missing(wd)) {
    step("Error: name of working directory not specified")
  }
  
  if (missing(in_dem)) {
    step("Error: file name for original DEM not specified")
  }
  
  
  if (is.null(threshold)) {
    step("Error: threshold for channel initiation not specified")
  }
  
  if (is.null(snap_dist)) {
    step("Error: maximum pour point snap distance not specified")
  }
  
  file_names <- ch_wbt_filenames(wd)
  # define 
  
  dem_ns <- ch_wbt_removesinks(in_dem = in_dem, out_dem = file_names$dem_ns, 
                               method = sink_method, dist = dist, 
                               fn_dem_fsc = file_names$dem_fsc, ...)

  if (inherits(dem_ns, "character")) return(NULL)
  ch_wbt_flow_accumulation(fn_dem_ns = file_names$dem_ns, fn_flowacc = file_names$flowacc,
                           return_raster = FALSE)
  ch_wbt_flow_direction(fn_dem_ns = file_names$dem_ns, fn_flowdir = file_names$flowdir,
                        return_raster = FALSE)
  wbt_extract_streams(file_names$flowacc, file_names$channel_ras, threshold = threshold)
  wbt_raster_streams_to_vector(file_names$channel_ras, file_names$flowdir, file_names$channel_vec)
  sf::st_write(pp_sf, file_names$pp, quiet = TRUE, delete_layer = TRUE)
  wbt_snap_pour_points(file_names$pp, file_names$flowacc, file_names$pp_snap, snap_dist)
  wbt_watershed(file_names$flowdir, file_names$pp_snap, file_names$catchment_ras)
  wbt_raster_to_vector_polygons(file_names$catchment_ras, file_names$catchment_vec)
  catchment_vec <- st_read(file_names$catchment_vec) %>% st_as_sf()
  if (is.na(sf::st_crs(catchment_vec))) {
    sf::st_crs(catchment_vec) <- sf::st_crs(raster(file_names$catchment_ras))
    sf::write_sf(catchment_vec, file_names$catchment_vec)
  }
  
  channel_vec <- st_read(file_names$channel_vec) %>% st_as_sf()
  if (is.na(sf::st_crs(channel_vec))) {
    sf::st_crs(channel_vec) <- sf::st_crs(catchment_vec)
    sf::write_sf(channel_vec, file_names$catchment_vec)
  }
  
  if (check_catchment) {
    result <- ch_checkcatchment(dem = dem_ns, catchment = catchment_vec, outlet = pp_sf, 
                      channel_vec = channel_vec, cb_colour = cb_colour, pp_colour = pp_colour, 
                      channel_colour = channel_colour, contour_colour = contour_colour, 
                      plot_na = plot_na, plot_scale = plot_scale,
                      na_location = na_location, scale_location = scale_location)
  }
  return(catchment_vec)
}