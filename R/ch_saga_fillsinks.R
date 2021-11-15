#' Fill DEM
#' 
#' @description
#' Removes sinks in a DEM by filling. This function is a wrapper to the
#' \pkg{RSAGA} function \code{rsaga.fill.sinks}.
#' 
#' @details
#' Removes sinks by filling them. To ensure continuous flow through the DEM, 
#' a minimum slope can be imposed on cells. Although there are other options, the most important 
#' are method and minslope; these are implemented in the function. For a full list of 
#' available options type \command{?rsaga.fill.sinks} at the console. 
#' The main input is a DEM as a raster object. The main output will be a sink-filled DEM with the 
#' same topology and coordinate reference system as the input raster.
#' 
#' @param dem_raw     raster object of your raw dem in the desired projection
#' @param saga_wd     working directory to write and read saga files
#' @param sinkmethod  options are \option{planchon.darboux.2001} (default), 
#' \option{xxl.wang.liu.2006} and \option{wang.liu.2006}
#' @param minslope    minimum slope to impose on filled cells to force drainage (default = 0.1)
#' @param saga.env    SAGA environment object. Default is to let saga find it on its own.
#' @return \item{dem_ns}{filled dem as a raster object.}
#' 
#' @importFrom RSAGA rsaga.fill.sinks 
#' @importFrom raster writeRaster raster crs extract
#' 
#' @author Dan Moore
#' @seealso \code{\link{ch_saga_removesinks}} to remove sinks instead of filling
#' @export
#' @examples
#' # create saga wd using base::tempdir()
#' saga_wd <- tempdir()
#' # use volcano DEM
#' dem <- ch_volcano_raster()
#' 
#' # fill sinks
#' filled_dem <-  ch_saga_fillsinks(dem_raw=dem, saga_wd=saga_wd)
#' 
#' # plot the difference in raw and filled dem (positive -> filled)
#' library(raster)
#' plot(filled_dem-dem)

ch_saga_fillsinks <- function(dem_raw, saga_wd, 
                              sinkmethod = "planchon.darboux.2001", 
                              minslope = 0.1,
                              saga.env = RSAGA::rsaga.env()) {
  
  # check inputs
  if (missing(dem_raw)) {
    stop("ch_saga_fillsinks requires a raster dem_raw")
  }
  if (missing(saga_wd)) {
    saga_wd <- tempdir()
    warning(sprintf("ch_saga_fillsinks: no saga_wd defined; setting temporary saga_wd with tempdir as:\n%s",saga_wd))
  }
  
  # error trap - saga_wd does not exist
  if (!dir.exists(saga_wd)) {
    print("Provided saga_wd does not exist")
    return(NA)
  }
  
  # store the input dem in a file in the working directory
  raster::writeRaster(dem_raw, paste0(saga_wd, "/dem_raw.sdat"), format = "SAGA", 
                      NAflag = -9999, overwrite = TRUE)
  # fill sinks 
  RSAGA::rsaga.fill.sinks(in.dem = paste0(saga_wd, "/dem_raw.sgrd"), 
                          out.dem = paste0(saga_wd, '/dem_ns.sgrd'), 
                          method = sinkmethod, minslope = minslope,
                          env = saga.env)
  # create filled dem as a raster object
  ns_file <- paste0(saga_wd, "/dem_ns.sdat")
  dem_ns <- raster::raster(ns_file, format = "SAGA")
  raster::crs(dem_ns) <- raster::crs(dem_raw)
  return(dem_ns)
}
