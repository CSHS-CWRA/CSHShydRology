#' Snap pour points to channels
#' 
#' @description Pour points describe the outlets of sub-basins within a DEM. To use
#' the pour points to delineate catchments, they must align with the drainage
#' network. This function snaps (forces the locations) of pour points to the
#' channels.
#' @param pp_sf \pkg{sf} object containing pour points. These must be supplied by the user. See
#' the code in \code{\link{ch_volcano_pourpoints}} for an example of creating the object.
#' @param fn_flowacc Name of file containing flow accumulations.
#' @param fn_pp File name to create un-snapped pour points.
#' @param fn_pp_snap File name for snapped pour points.
#' @param check_crs If \code{TRUE} the projections of the pour points and flow
#' accumulation files will be checked to ensure they are identical.
#' @param snap_dist Maximum snap distance in map units. 
#' @param ... Additional parameters for \pkg{whitebox} function \code{wbt_snap_pour_points}.
#'
#' @author Dan Moore
#' @seealso \code{\link{ch_volcano_pourpoints}}
#' @importFrom raster raster
#' @importFrom whitebox wbt_snap_pour_points
#' @importFrom sf st_crs st_write
#' @return Returns a \pkg{sf} object of the specified pour points snapped to the
#' channel network.
#' @export 
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
#'   # get pour points
#'   pourpoint_file <- tempfile("volcano_pourpoints", fileext = c(".shp"))
#'   pourpoints <- ch_volcano_pourpoints(pourpoint_file)
#'   snapped_pourpoint_file <- tempfile("snapped_pourpoints", fileext = c(".shp"))
#'   snapped_pourpoints <- ch_wbt_pourpoints(pourpoints, flow_acc_file, pourpoint_file,
#'   snapped_pourpoint_file, snap_dist = 10)
#' } else {
#'   message("Examples not run as Whitebox executable not found")
#' }
ch_wbt_pourpoints <- function(pp_sf = NULL, fn_flowacc, fn_pp, fn_pp_snap, 
                              check_crs = TRUE, snap_dist = NULL, ...) {
  ch_wbt_check_whitebox()
  if (!file.exists(fn_flowacc)) {
    stop("Error: flow accumulation file does not exist")
  }
  if (missing(pp_sf)) {
    stop("Error: value for pp_sf missing")
  }
  if (is.null(snap_dist)) {
    stop("Error: value for snap_dist missing")
  }
  if (check_crs) {
    pp_crs <- st_crs(pp_sf)
    fa_crs <- st_crs(raster(fn_flowacc))
    if (pp_crs != fa_crs) {
      stop("Error: pour points and flow accumulation grid have different crs")
    }
  }
  message("ch_wbt: Snapping pour points to stream network")
  st_write(pp_sf, fn_pp, quiet = TRUE, delete_layer = TRUE)
  wbt_snap_pour_points(fn_pp, fn_flowacc, fn_pp_snap, snap_dist, ...)
  return(st_read(fn_pp_snap))
}
