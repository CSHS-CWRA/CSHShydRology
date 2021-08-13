#' Deepen Drainage Network
#' 
#' Removes sinks by deepening drainage network; alternative to ch_saga_fillsinks. This function acts as a wrapper to 
#' the `rsaga.sink.removal` function.
#' 
#' @param dem_raw Raster object of your raw dem in the desired projection
#' @param saga_wd     working directory to write and read saga files
#' @param saga_env    SAGA environment object.  Default is to let saga find it on its own.
#' @return {dem_ns}{processed dem as a raster object.}
#' 
#' @importFrom RSAGA rsaga.sink.removal 
#' @importFrom raster writeRaster raster crs extract
#' 
#' @author Dan Moore <dan.moore@ubc.ca>
#' @seealso \code{\link{ch_saga_fillsinks}} to fill sinks instead of removing
#' @export
#' @examples
#' \dontrun{
#' # note: example not run in package compilation
#' # - requires creating and accessing a temporary directory
#' # - requires downloading spatial data from Zenodo repository
#' # - requires a potentially lengthy GIS operation
#' 
#' # create saga wd using base::tempdir()
#' saga_wd <- tempdir()
#'
#' # download 25m DEM
#' ff <- "gs_dem25.tif"
#' ra_fn <- file.path(saga_wd, ff)
#' ra_url <- sprintf("https://zenodo.org/record/4781469/files/%s",ff)
#' dem <- ch_get_url_data(ra_url, ra_fn)
#' 
#' # remove sinks
#' removed_dem <-  ch_saga_removesinks(dem_raw=dem, saga_wd=saga_wd)
#' 
#' # plot the difference in raw and sink-removed dem 
#' library(raster)
#' plot(removed_dem-dem)
#' }
#' 
ch_saga_removesinks <- function(dem_raw, saga_wd, 
                                saga_env = RSAGA::rsaga.env()) {
  
  # error trap - saga_wd does not exist
  if (!dir.exists(saga_wd)) {
    print("saga_wd does not exist")
    return(NA)
  }
  # store the input dem in a file in the working directory
  raster::writeRaster(dem_raw, paste0(saga_wd, "/dem_raw.sdat"), format = "SAGA", 
                      NAflag = -9999, overwrite = TRUE)
  # remove sinks 
  RSAGA::rsaga.sink.removal(in.dem = paste0(saga_wd, "/dem_raw.sgrd"), 
                            out.dem = paste0(saga_wd, '/dem_ns.sgrd'), 
                            method = "deepen drainage route",
                            env = saga_env)
  # create filled dem as a raster object
  ns_file <- paste0(saga_wd, "/dem_ns.sdat")
  dem_ns <- raster::raster(ns_file, format = "SAGA")
  raster::crs(dem_ns) <- raster::crs(dem_raw)
  return(dem_ns)
}

