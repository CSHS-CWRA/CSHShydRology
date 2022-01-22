# Creates default names for Whitebox function input and output files

#'
#' @param wd 
#' @param fn_dem 
#' @param fn_dem_fsc 
#' @param fn_dem_ns 
#' @param fn_flowacc 
#' @param fn_flowdir 
#' @param fn_channel_ras 
#' @param fn_channel_vec 
#' @param fn_catchment_ras 
#' @param fn_catchment_vec 
#' @param fn_pp 
#' @param fn_pp_snap 
#'
#' @return Returns a list of the input and output file names
#' @export
#'
#' @examples
ch_wbt_filenames <- function(
  wd = wd_name, 
  fn_dem = "dem.tif",
  fn_dem_fsc = "dem_fsc.tif",
  fn_dem_ns = "dem_ns.tif",
  fn_flowacc = "flow_acc.tif",
  fn_flowdir = "flow_dir.tif",
  fn_channel_ras = "channel.tif",
  fn_channel_vec = "channel.shp",
  fn_catchment_ras = "catchment.tif",
  fn_catchment_vec = "catchment.shp",
  fn_pp = "pp.shp",
  fn_pp_snap = "pp_snap.shp") {
  fn_list <- list(
    dem = file.path(wd, fn_dem),
    dem_fsc = file.path(wd, fn_dem_fsc),
    dem_ns = file.path(wd, fn_dem_ns),
    flowacc = file.path(wd, fn_flowacc),
    flowdir = file.path(wd, fn_flowdir),
    channel_ras = file.path(wd, fn_channel_ras),
    catchment_ras = file.path(wd, fn_catchment_ras),
    channel_vec = file.path(wd, fn_channel_vec),
    catchment_vec = file.path(wd, fn_catchment_vec),
    pp = file.path(wd, fn_pp),
    pp_snap = file.path(wd, fn_pp_snap)
  )
  return(fn_list)
}