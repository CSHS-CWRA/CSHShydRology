#' Removes sinks from a DEM
#' 
#' @description Sinks are removed from a DEM using one of several methods. The raster file
#' types supported are listed in \code{\link{SpatialHydrology-functions}}.
#'
#' @param in_dem File path for original dem. Required.
#' @param out_dem File path for dem after removing sinks.
#' @param method Method for removing sinks. Default method is \option{breach_leastcost}. Other methods include
#' \option{breach}, \option{fill}, \option{fill_pd} (Planchot and Darboux), and \option{fill_wl} (Wang and Liu).
#' 
#' @param dist Maximum search distance for breach paths in cells. Required if \code{method = "breach_leastcost"}.
#' @param fn_dem_fsc 
#' @param ... 
#'
#' @author Dan Moore
#' @importFrom raster raster
#' @importFrom whitebox wbt_fill_single_cell_pits wbt_breach_depressions_least_cost wbt_fill_depressions_wang_and_liu
#' @importFrom whitebox wbt_breach_depressions wbt_fill_depressions wbt_fill_depressions_planchon_and_darboux
#' @return Returns 
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