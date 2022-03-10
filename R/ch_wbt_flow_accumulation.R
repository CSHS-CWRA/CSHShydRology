#' Creates flow accumulation grid file
#'
#' @param fn_dem_ns File name of dem with sinks removed.
#' @param fn_flowacc File name for flow accumulation grid to be created.
#' @param return_raster If \code{TRUE} (the default), the flow accumulation
#' grid will be returned as a raster object, in addition to being written to
#' \option{fn_flowacc}. If \code{FALSE}, the output file will still be created
#' but a \code{NULL} value is returned.
#'
#' @author Dan Moore
#' @importFrom raster raster
#' @importFrom whitebox wbt_d8_flow_accumulation
#' @return If \code{return_raster = TRUE}, the flow accumulation
#' grid will be returned as a raster object, otherwise \code{NULL} is returned.
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
#' # get flow accumulations
#' flow_acc_file <- tempfile("flow_acc", fileext = c(".tif"))
#' flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)
#' plot(flow_acc)
#' }
ch_wbt_flow_accumulation <- function(fn_dem_ns, fn_flowacc, return_raster = TRUE) {
  ch_wbt_check_whitebox()
  if (!file.exists(fn_dem_ns)) {
    stop("Error: input sink-free dem file does not exist")
  }
  
  message("ch_wbt: Creating flow accumulation grid")
  wbt_d8_flow_accumulation(fn_dem_ns, fn_flowacc)
  
  if (return_raster) {
    return(raster(fn_flowacc))
  } else {
    return(NULL)
  }
}
