#' Removes sinks from a DEM
#'
#' @param in_dem 
#' @param out_dem 
#' @param method 
#' @param dist 
#' @param fn_dem_fsc 
#' @param ... 
#'
#' @author Dan Moore
#' @importFrom raster raster
#' @importFrom whitebox wbt_fill_single_cell_pits wbt_breach_depressions_least_cost wbt_fill_depressions_wang_and_liu
#' @importFrom whitebox wbt_breach_depressions wbt_fill_depressions wbt_fill_depressions_planchon_and_darboux
#' @return
#' @export
#'
#' @examples
ch_wbt_removesinks <- function(in_dem, out_dem, method = "breach_leastcost", 
                               dist = NULL, fn_dem_fsc = NULL, ...) {
  if (!file.exists(in_dem)) {
    stop("Error: input dem file does not exist")
  }
  if (method == "breach_leastcost") {
    if (is.null(dist)) {
      stop("Error: no value for dist, which is required for wbt_breach_depressions_least_cost")
    }
    wbt_fill_single_cell_pits(in_dem, fn_dem_fsc)
    wbt_breach_depressions_least_cost(fn_dem_fsc, out_dem, dist, ...)
  } else if (method == "breach") {
    wbt_fill_single_cell_pits(in_dem, fn_dem_fsc)
    wbt_breach_depressions(fn_dem_fsc, out_dem, ...)
  } else if (method == "fill") {
    wbt_fill_depressions(in_dem, out_dem, ...)
  } else if (method == "fill_pd") {
    wbt_fill_depressions_planchon_and_darboux(in_dem, out_dem, ...)
  } else if (method == "fill_wl") {
    wbt_fill_depressions_wang_and_liu(in_dem, out_dem, ...)
  } else {
    stop("Error: incorrect method for sink removal specified")
  }
  return(raster(out_dem))
}