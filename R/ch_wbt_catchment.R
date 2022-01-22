#' Delineate catchment boundaries
#'
#' @param fn_pp_snap 
#' @param fn_flowdir 
#' @param fn_catchment_ras 
#' @param fn_catchment_vec 
#' @param return_vector 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
ch_wbt_catchment <- function(fn_pp_snap, fn_flowdir, fn_catchment_ras, 
                             fn_catchment_vec, return_vector = TRUE, ...) {
  if (!file.exists(fn_pp_snap)) {
    msg <- "Error: file containing snapped pour points does not exist"
    print(msg)
    return(msg)
  }
  if (!file.exists(fn_flowdir)) {
    msg <- "Error: input flow direction file does not exist"
    print(msg)
    return(msg)
  }
  print("ch_wbt: Delineating catchment boundaries")
  crs_pp <- st_crs(st_read(fn_pp_snap))$epsg
  crs_fd <- st_crs(raster::raster(fn_flowdir))$epsg
  if (crs_pp != crs_fd) {
    msg <- "Error: pour points and flow direction grid have different crs"
    print(msg)
    return(msg)
  }
  whitebox::wbt_watershed(d8_pntr = fn_flowdir, pour_pts = fn_pp_snap, 
                          output = fn_catchment_ras)
  whitebox::wbt_raster_to_vector_polygons(fn$catchment_ras, fn$catchment_vec)
  catchment_vec <- st_read(fn_catchment_vec) %>% st_as_sf()
  if(is.na(st_crs(catchment_vec))){
    st_crs(catchment_vec) <- st_crs(raster(fn_catchment_ras))
    write_sf(catchment_vec, fn_catchment_vec)
  }
  if (return_vector) {
    return(st_read(fn_catchment_vec))
  } else {
    return()
  }
}
