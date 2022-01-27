#' Creates flow direction grid file
#'
#' @param fn_dem_ns 
#' @param fn_flowdir 
#' @param return_raster 
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
