#' Title
#'
#' @param wd 
#' @param in_dem 
#' @param pp_sf 
#' @param sink_method 
#' @param dist 
#' @param check_catchment 
#' @param threshold 
#' @param snap_dist 
#' @param cb_colour 
#' @param pp_colour 
#' @param channel_colour 
#' @param contour_colour 
#' @param plot_na 
#' @param plot_scale 
#' @param na_location 
#' @param scale_location 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
ch_wbt_catchment_onestep <- function(wd, in_dem, pp_sf, 
                                     sink_method = "breach_leastcost", dist = NULL, 
                                     check_catchment = TRUE, threshold = NULL, snap_dist = NULL, 
                                     cb_colour = "red", pp_colour = "red",
                                     channel_colour = "blue", contour_colour = "grey",       
                                     plot_na = TRUE, plot_scale = TRUE,
                                     na_location = "bl", scale_location = "bl", ...) {
  
  fn <- ch_wbt_filenames(wd)
  dem_ns <- ch_wbt_removesinks(in_dem = in_dem, out_dem = fn$dem_ns, 
                               method = sink_method, dist = dist, 
                               fn_dem_fsc = fn$dem_fsc, ...)
  if (class(dem_ns) == "character")  return(NULL)
  ch_wbt_flow_accumulation(fn_dem = fn$dem_ns, fn_flowacc = fn$flowacc,
                           return_raster = FALSE)
  ch_wbt_flow_direction(fn_dem = fn$dem_ns, fn_flowdir = fn$flowdir,
                        return_raster = FALSE)
  whitebox::wbt_extract_streams(fn$flowacc, fn$channel_ras, threshold = threshold)
  whitebox::wbt_raster_streams_to_vector(fn$channel_ras, fn$flowdir, fn$channel_vec)
  st_write(pp_sf, fn$pp, quiet = TRUE, delete_layer = TRUE)
  whitebox::wbt_snap_pour_points(fn$pp, fn$flowacc, fn$pp_snap, snap_dist)
  whitebox::wbt_watershed(fn$flowdir, fn$pp_snap, fn$catchment_ras)
  whitebox::wbt_raster_to_vector_polygons(fn$catchment_ras, fn$catchment_vec)
  catchment_vec <- st_read(fn$catchment_vec) %>% st_as_sf()
  if(is.na(st_crs(catchment_vec))){
    st_crs(catchment_vec) <- st_crs(raster(fn_catchment_ras))
    write_sf(catchment_vec, fn$catchment_vec)
  }
  
  channel_vec <- st_read(fn$channel_vec) %>% st_as_sf()
  if (is.na(st_crs(channel_vec))) {
    st_crs(channel_vec) <- st_crs(catchment_vec)
    write_sf(channel_vec, fn$catchment_vec)
  }
  
  if (check_catchment) {
    ch_checkcatchment(dem = dem_ns, catchment = catchment_vec, outlet = pp_sf, 
                      channel_vec = channel_vec, cb_colour = cb_colour, pp_colour = pp_colour, 
                      channel_colour = channel_colour, contour_colour = contour_colour, 
                      plot_na = plot_na, plot_scale = plot_scale,
                      na_location = na_location, scale_location = scale_location)
  }
  return(catchment_vec)
}