#' Contributing Area Grid
#' 
#' Generates a grid of contributing area for each grid cell.
#' 
#' This function generates a raster of contributing areas based on a DEM, 
#' which should have had sinks removed in a pre-processing step. For more information, see 
#' ?`rsaga.topdown.processing`.
#' 
#' @param dem     Raster object of your raw dem in the desired projection
#' @param saga_wd     working directory to write and read saga files
#' @param method      character or numeric: choice of processing algorithm (default "mfd", or 4)
#' @param linear_threshold 	numeric (number of grid cells): threshold above which linear flow (i.e. the Deterministic 8 algorithm) will be used; linear flow is disabled for linear_threshold=Inf (the default)
#' @param saga_env    SAGA environment object.  Default is to let saga find it on its own.
#' @return  
#' \item{carea}{raster of contributing areas for each pixel.}
#' 
#' @importFrom RSAGA rsaga.topdown.processing
#' @importFrom raster writeRaster raster crs 
#' 
#' @author Dan Moore <dan.moore@ubc.ca>
#' @seealso \code{\link{ch_saga_fillsinks}} to fill sinks instead of removing
#' @seealso \code{\link{rsaga.topdown.processing}} for more information
#' @export
#' @examples
#' \dontrun{
#' ch_saga_carea()
#' }
#' 
ch_saga_carea <- function(dem, saga_wd, 
                          method = 4,
                          linear_threshold = Inf,
                          saga_env = RSAGA::rsaga.env()) {
  
  # require(RSAGA)
  # require(raster)
  # require(sf)
  
  # error trap - saga_wd does not exist
  if (!dir.exists(saga_wd)) {
    print("saga_wd does not exist")
    return(NA)
  }
  # store the dem object in the working directory
  raster::writeRaster(dem, paste0(saga_wd, "/dem.sdat"), format = "SAGA", 
                      NAflag = -9999, overwrite = TRUE)
  # generate contributing area grid
  RSAGA::rsaga.topdown.processing(paste0(saga_wd, "/dem.sgrd"), 
                                  out.carea = paste0(saga_wd, "/carea.sgrd"), 
                                  method = method,
                                  linear.threshold = linear_threshold,
                                  env = saga_env)
  # read the catchment area grid into a raster object and return
  carea <- raster::raster(paste0(saga_wd, '/carea.sdat'))
  raster::crs(carea) <- raster::crs(dem)
  return(carea)
}
