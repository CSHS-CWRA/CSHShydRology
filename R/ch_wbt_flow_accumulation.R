# Flow accumulation grid
#'
#' @param fn_dem_ns 
#' @param fn_flowacc 
#' @param return_raster 
#'
#' @return
#' @export
#'
#' @examples
ch_wbt_flow_accumulation <- function(fn_dem_ns, fn_flowacc, return_raster = TRUE) {
  if (!file.exists(fn_dem_ns)) {
    msg <- "Error: input sink-free dem file does not exist"
    print(msg)
    return(msg)
  }
  print("ch_wbt: Creating flow accumulation grid")
  whitebox::wbt_d8_flow_accumulation(fn_dem_ns, fn_flowacc)
  if (return_raster) {
    return(raster::raster(fn_flowacc))
  } else {
    return(NULL)
  }
}