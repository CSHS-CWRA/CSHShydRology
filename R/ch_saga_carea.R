#' Contributing Area Grid
#' 
#' @description 
#' Generates a grid of contributing area for each grid cell.
#' 
#' @details
#' Generates a raster of contributing areas based on a DEM, 
#' which should have had sinks removed in a pre-processing step. For more information, see 
#' \command{?rsaga.topdown.processing}.
#' 
#' @param dem raster object of your dem in the desired projection - should have had sinks removed
#' @param saga_wd     working directory to write and read saga files
#' @param method      character or numeric: choice of processing algorithm (default \option{mfd}, or \code{4})
#' @param linear_threshold 	numeric (number of grid cells): threshold above which 
#' linear flow (i.e. the Deterministic 8 algorithm) will be used; linear flow is 
#' disabled for \code{linear_threshold = Inf} (the default)
#' @param saga_env    SAGA environment object. Default is to let saga find it on its own.
#' @return  
#' \item{carea}{raster of contributing areas for each pixel.}
#' 
#' @importFrom RSAGA rsaga.topdown.processing
#' @importFrom raster writeRaster raster crs 
#' 
#' @author Dan Moore
#' @seealso \code{\link{ch_saga_fillsinks}} to fill sinks instead of removing
#' @seealso \code{\link[RSAGA]{rsaga.topdown.processing}} for more information
#' @export
#' @examples
#' # These examples are not executed if the installed version of 
#' # SAGA is outside the allowed range of 2.3.1 - 6.3.0
#' # as calling RSAGA functions will cause warnings
#' library(RSAGA)
#' saga_env <- rsaga.env()
#' version <- saga_env$version
#' if ((version >= "2.3.1") & (version <= "6.3.0")) {
#' # create saga wd using tempdir()
#' saga_wd <- tempdir()
#' # use volcano DEM
#' dem <- ch_volcano_raster()
#' 
#' # fill sinks
#' filled_dem <-  ch_saga_fillsinks(dem_raw=dem, saga_wd=saga_wd)
#' 
#' # determine contributing area raster using filled_dem
#' carea <- ch_saga_carea(filled_dem, saga_wd)
#' 
#' # plot contributing area raster
#' library(raster)
#' plot(carea)
#' }
ch_saga_carea <- function(dem, saga_wd, 
                          method = 4,
                          linear_threshold = Inf,
                          saga_env = RSAGA::rsaga.env()) {
  
  # check inputs
  if (missing(dem)) {
    stop("ch_saga_carea requires a raster dem")
  }
  if (missing(saga_wd)) {
    saga_wd <- tempdir()
    warning(sprintf("ch_saga_carea: no saga_wd defined; setting temporary saga_wd with tempdir as:\n%s",saga_wd))
  }
  
  # error trap - saga_wd does not exist
  if (!dir.exists(saga_wd)) {
    print("Provided saga_wd does not exist")
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
