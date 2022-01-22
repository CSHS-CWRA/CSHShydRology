#' Snap pour points to channels
#' @param pp_sf 
#' @param fn_flowacc 
#' @param fn_pp 
#' @param fn_pp_snap 
#' @param check_crs 
#' @param snap_dist 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
ch_wbt_pourpoints <- function(pp_sf = NULL, fn_flowacc, fn_pp, fn_pp_snap, 
                              check_crs = TRUE, snap_dist = NULL, ...) {
  if (!file.exists(fn_flowacc)) {
    msg <- "Error: flow accumulation file does not exist"
    print(msg)
    return(msg)
  }
  if (is.null(pp_sf)) {
    msg <- "Error: value for pp_sf missing"
    print(msg)
    return(msg)
  }
  if (is.null(snap_dist)) {
    msg <- "Error: value for snap_dist missing"
    print(msg)
    return(msg)
  }
  if (check_crs) {
    pp_crs <- st_crs(pp_sf)$epsg
    fa_crs <- st_crs(raster::raster(fn_flowacc))$epsg
    if (pp_crs != fa_crs) {
      msg <- "Error: pour points and flow accumulation grid have different crs"
      print(msg)
      return(msg)
    }
  }
  print("ch_wbt: Snapping pour points to stream network")
  st_write(pp_sf, fn_pp, quiet = TRUE, delete_layer = TRUE)
  whitebox::wbt_snap_pour_points(fn_pp, fn_flowacc, fn_pp_snap, snap_dist, ...)
  return(st_read(fn_pp_snap))
}
