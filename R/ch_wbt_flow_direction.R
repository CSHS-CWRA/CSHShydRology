#' Flow direction grid
#'
#' @param fn_dem_ns 
#' @param fn_flowdir 
#' @param return_raster 
#'
#' @return
#' @export
#'
#' @examples
ch_wbt_flow_direction <- function(fn_dem_ns, fn_flowdir, return_raster = TRUE) {
  if (!file.exists(fn_dem_ns)) {
    msg <- "Error: input sink-free dem file does not exist"
    print(msg)
    return(msg)
  }
  print("ch_wbt: Creating flow direction grid")
  whitebox::wbt_d8_pointer(fn_dem_ns, fn_flowdir)
  if (return_raster) {
    return(raster::raster(fn_flowdir))
  } else {
    return(NULL)
  }
}
