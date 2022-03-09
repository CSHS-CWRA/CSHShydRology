#' Creates flow direction grid file
#'
#' @param fn_dem_ns File name of dem with sinks removed.
#' @param fn_flowdir File name for flow direction grid to be created.
#' @param return_raster Should a raster object be returned?
#'
#' @author Dan Moore
#' @importFrom raster raster
#' @importFrom whitebox wbt_d8_pointer
#' @return If \code{return_raster = TRUE} (the default), the flow direction
#' grid will be returned as a raster object, in addition to being written to
#' \option{fn_flowdir}. If \code{return_raster = FALSE}, the output file will still be created
#' but a \code{NULL} value is returned.
#' @export
#'
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
#' # get flow directions
#' flow_dir_file <- tempfile("flow_dir", fileext = c(".tif"))
#' flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
#' plot(flow_dir)
#' }
ch_wbt_flow_direction <- function(fn_dem_ns, fn_flowdir, return_raster = TRUE) {
  ch_wbt_check_whitebox()
  if (!file.exists(fn_dem_ns)) {
    stop("Error: input sink-free dem file does not exist")
  }
  message("ch_wbt: Creating flow direction grid")
  
  wbt_d8_pointer(fn_dem_ns, fn_flowdir)
  if (return_raster) {
    return(raster(fn_flowdir))
  } else {
    return(NULL)
  }
}
