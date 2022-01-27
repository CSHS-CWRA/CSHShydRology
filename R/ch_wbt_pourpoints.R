#' Snap pour points to channels
#' @param pp_sf 
#' @param fn_flowacc Name of file containing flow accumulations.
#' @param fn_pp 
#' @param fn_pp_snap 
#' @param check_crs If \code{TRUE} the projections of the pour points and flow
#' accumulation files will be checked to ensure they are identical.
#' @param snap_dist 
#' @param ... 
#'
#' @author Dan Moore
#' @importFrom raster raster
#' @importFrom whitebox wbt_snap_pour_points
#' @importFrom sf st_crs st_write
#' @return
#' @export 
#'
#' @examples
ch_wbt_pourpoints <- function(pp_sf = NULL, fn_flowacc, fn_pp, fn_pp_snap, 
                              check_crs = TRUE, snap_dist = NULL, ...) {
  if (!file.exists(fn_flowacc)) {
    stop("Error: flow accumulation file does not exist")
  }
  if (is.null(pp_sf)) {
    stop("Error: value for pp_sf missing")
  }
  if (is.null(snap_dist)) {
    stop("Error: value for snap_dist missing")
  }
  if (check_crs) {
    pp_crs <- st_crs(pp_sf)$epsg
    fa_crs <- st_crs(raster(fn_flowacc))$epsg
    if (pp_crs != fa_crs) {
      stop("Error: pour points and flow accumulation grid have different crs")
    }
  }
  message("ch_wbt: Snapping pour points to stream network")
  st_write(pp_sf, fn_pp, quiet = TRUE, delete_layer = TRUE)
  wbt_snap_pour_points(fn_pp, fn_flowacc, fn_pp_snap, snap_dist, ...)
  return(st_read(fn_pp_snap))
}
