#' Creates names for Whitebox function input and output files
#' 
#' @description Creates a list of the files used for inputs and outputs by the
#' Whitebox functions. This function needs to be called before calling any of the other
#' Whitebox (i.e. those prefixed by \code{cd_wbt}) functions. If the file names are not specified, default names will 
#' be used. All raster files are TIFF (.tif), all vector files are shapefiles (.shp).
#' @param wd Required. Name of working directory.
#' @param fn_dem File name of input DEM. Default is \option{dem.tif}.
#' @param fn_dem_fsc File name for dem after filling single-cell pits. Default is \option{dem_fsc.tif}.
#' @param fn_dem_ns File name for dem removing sinks. Default is \option{dem_ns.tif}.
#' @param fn_flowacc File name for DEM flow accumulation grid Default is \option{flow_acc.tif}.
#' @param fn_flowdir File name for DEM flow direction grid. Default is \option{flow_dir.tif}.
#' @param fn_channel_ras File name for raster version of channel network. Default is \option{channel.tif}. 
#' @param fn_channel_vec File name for vector version of channel networks. Default is \option{channel.shp}. 
#' @param fn_catchment_ras File name for raster version of catchment. Default is \option{catchment.tif}. 
#' @param fn_catchment_vec File name for vector version of catchment. Default is \option{catchment.shp}. 
#' @param fn_pp File name for pour points (input). Vector file. Default is \option{pp.shp}.
#' @param fn_pp_snap File name for pour points after snapping to channel network. Vector file. Default is \option{pp.shp}.
#'
#' @author Dan Moore
#' @return Returns a list of the input and output file names
#' @export
#'
#' @examples
#' wbt_file_names <- ch_wbt_filenames(getwd())
#' 
ch_wbt_filenames <- function(
  wd = NULL, 
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
  
  if (is.null(wd)) {
    stop("Working directory not specified")
  }
  
  if (!file.exists(wd)) {
    stop("Working directory does not exist")
  }
  
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