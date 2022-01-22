#' Removes sinks from a DEM
#'
#' @param in_dem 
#' @param out_dem 
#' @param method 
#' @param dist 
#' @param fn_dem_fsc 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
ch_wbt_removesinks <- function(in_dem, out_dem, method = "breach_leastcost", 
                               dist = NULL, fn_dem_fsc = NULL, ...) {
  if (!file.exists(in_dem)) {
    msg <- "Error: input dem file does not exist"
    print(msg)
    return(msg)
  }
  if (method == "breach_leastcost") {
    if (is.null(dist)) {
      msg <- paste("Error: no value for dist, which is required for",
                   "wbt_breach_depressions_least_cost")
      print(msg)
      return(msg)
    }
    whitebox::wbt_fill_single_cell_pits(in_dem, fn_dem_fsc)
    whitebox::wbt_breach_depressions_least_cost(fn_dem_fsc, out_dem, dist, ...)
  } else if (method == "breach") {
    whitebox::wbt_fill_single_cell_pits(in_dem, fn_dem_fsc)
    whitebox::wbt_breach_depressions(fn_dem_fsc, out_dem, ...)
  } else if (method == "fill") {
    whitebox::wbt_fill_depressions(in_dem, out_dem, ...)
  } else if (method == "fill_pd") {
    whitebox::wbt_fill_depressions_planchon_and_darboux(in_dem, out_dem, ...)
  } else if (method == "fill_wl") {
    whitebox::wbt_fill_depressions_wang_and_liu(in_dem, out_dem, ...)
  } else {
    msg <- "Error: incorrect method for sink removal specified"
    print(msg)
    return(msg)
  }
  return(raster::raster(out_dem))
}