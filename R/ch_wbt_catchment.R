#' Delineate catchment boundaries
#'
#' @param fn_pp_snap Name of file containing snapped pour points
#' @param fn_flowdir Name of file containing flow accumulations.
#' @param fn_catchment_ras Raster file to contain delineated catchment.
#' @param fn_catchment_vec Vector file to contain delineated catchment.
#' @param return_vector If \code{TRUE} (the default) a vector of the catchment will be returned.
#' 
#' @author Dan Moore and Kevin Shook
#' @importFrom raster raster
#' @importFrom whitebox wbt_watershed wbt_raster_to_vector_polygons
#' @importFrom sf st_crs write_sf st_read
#' @return If \code{return_vector == TRUE} a vector of the catchment is returned. Otherwise
#' nothing is returned.
#' @export
#' @seealso  \code{\link{ch_wbt_catchment_onestep}}
#' @examples
#' \donttest{
#' # Not tested automatically as requires installation of Whitebox
#' library(raster)
#' test_raster <- ch_volcano_raster()
#' dem_raster_file <- tempfile(fileext = ".tif")
#' no_sink_raster_file <- tempfile("no_sinks", fileext = ".tif")
#' 
#' # write test raster to file
#' writeRaster(test_raster, dem_raster_file, format = "GTiff")
#' 
#' # remove sinks
#' removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, method = "fill")
#' 
#' # get flow accumulations
#' flow_acc_file <- tempfile("flow_acc", fileext = ".tif")
#' flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)
#' 
#' # get pour points
#' pourpoint_file <- tempfile("volcano_pourpoints", fileext = ".shp")
#' pourpoints <- ch_volcano_pourpoints(pourpoint_file)
#' snapped_pourpoint_file <- tempfile("snapped_pourpoints", fileext = ".shp")
#' snapped_pourpoints <- ch_wbt_pourpoints(pourpoints, flow_acc_file, pourpoint_file,
#' snapped_pourpoint_file, snap_dist = 10)
#' 
#' # get flow directions
#' flow_dir_file <- tempfile("flow_dir", fileext = ".tif")
#' flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
#' fn_catchment_ras <- tempfile("catchment", fileext = ".tif")
#' fn_catchment_vec <- tempfile("catchment", fileext = ".shp")
#' catchments <- ch_wbt_catchment(snapped_pourpoint_file, flow_dir_file, 
#' fn_catchment_ras, fn_catchment_vec)
#' }
ch_wbt_catchment <- function(fn_pp_snap, fn_flowdir, fn_catchment_ras, 
                             fn_catchment_vec, return_vector = TRUE) {
  ch_wbt_check_whitebox()
  if (!file.exists(fn_pp_snap)) {
    stop("Error: file containing snapped pour points does not exist")
  }
  if (!file.exists(fn_flowdir)) {
    stop("Error: input flow direction file does not exist")
  }
  
  message("ch_wbt: Delineating catchment boundaries")
  crs_pp <- sf::st_crs(st_read(fn_pp_snap))$epsg
  crs_fd <- sf::st_crs(raster(fn_flowdir))$epsg
  if (crs_pp != crs_fd) {
    stop("Error: pour points and flow direction grid have different crs")
  }
  wbt_watershed(d8_pntr = fn_flowdir, pour_pts = fn_pp_snap, 
                          output = fn_catchment_ras)
  wbt_raster_to_vector_polygons(fn_catchment_ras, fn_catchment_vec)
  catchment_vec <- st_read(fn_catchment_vec) %>% st_as_sf()
  if (is.na(st_crs(catchment_vec))) {
    sf::st_crs(catchment_vec) <- st_crs(raster(fn_catchment_ras))
    write_sf(catchment_vec, fn_catchment_vec)
  }
  if (return_vector) {
    return(st_read(fn_catchment_vec))
  } else {
    return()
  }
}
