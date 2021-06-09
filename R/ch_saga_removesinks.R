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
#' ch_saga_removesinks()
#' 
#' # consider sample DEM data with this
#  # https://github.com/wcmbishop/rayshader-demo/blob/master/R/elevation-api.R
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

