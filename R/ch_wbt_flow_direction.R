#' Creates flow direction grid file
#'
#' @param fn_dem_ns File name of dem with sinks removed.
#' @param fn_flowdir File name for flow direction grid to be created.
#' @param return_raster If \code{TRUE} (the default), the flow direction
#' grid will be returned as a raster object, in addition to being written to
#' \option{fn_flowdir}. If \code{FALSE}, the output file will still be created
#' but a \code{NULL} value is returned.
#'
#' @author Dan Moore
#' @importFrom raster raster
#' @importFrom whitebox wbt_d8_pointer
#' @return
#' @export
#'
#' @examples
ch_wbt_flow_direction <- function(fn_dem_ns, fn_flowdir, return_raster = TRUE) {
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
