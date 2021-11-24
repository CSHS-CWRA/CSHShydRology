#' Remove Sinks by Deepening the Drainage Network
#' 
#' @description 
#' Removes sinks by deepening drainage network; alternative to \code{ch_saga_fillsinks}. This 
#' function acts as a wrapper to 
#' the \code{rsaga.sink.removal} function.
#' 
#' @param dem_raw  raster object of your raw dem in the desired projection
#' @param saga_wd  working directory to write and read saga files
#' @param saga_env SAGA environment object. Default is to let SAGA find it on its own.
#' @return
#' \item{dem_ns}{processed dem as a raster object.}
#' 
#' @importFrom RSAGA rsaga.sink.removal rsaga.env
#' @importFrom raster writeRaster raster crs extract
#' 
#' @author Dan Moore
#' @seealso \code{\link{ch_saga_fillsinks}} to fill sinks in a DEM (instead of removing)
#' @export
#' @examples
#' # These examples are not executed if the installed version of 
#' # SAGA is outside the allowed range of 2.3.1 - 6.3.0
#' # as calling RSAGA functions will cause warnings
#' library(RSAGA)
#' saga_env <- rsaga.env()
#' version <- saga_env$version
#' if ((version >= "2.3.1") & (version <= "6.3.0")) {
#' # create saga wd using base::tempdir()
#' saga_wd <- tempdir()
#'
#' # use volcano DEM
#' dem <- ch_volcano_raster()
#' 
#' # remove sinks
#' removed_dem <- ch_saga_removesinks(dem_raw=dem, saga_wd=saga_wd)
#' # plot the difference in raw and sink-removed dem 
#' library(raster)
#' plot(removed_dem-dem)
#' }
ch_saga_removesinks <- function(dem_raw, saga_wd, 
                                saga_env = RSAGA::rsaga.env()) {
  # check inputs
  if (missing(dem_raw)) {
    stop("ch_saga_removesinks requires a raster dem_raw")
  }
  if (missing(saga_wd)) {
    saga_wd <- tempdir()
    warning(sprintf("ch_saga_removesinks: no saga_wd defined; setting temporary saga_wd with tempdir as:\n%s",saga_wd))
  }
  
  # error trap - saga_wd does not exist
  if (!dir.exists(saga_wd)) {
    print("Provided saga_wd does not exist")
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

