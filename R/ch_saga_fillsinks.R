#' Fill DEM
#' 
#' This removes sinks in a DEM by filling
#' 
#' @param dem_raw Raster object of your raw dem in the desired projection
#' @param saga_wd     working directory to write and read saga files
#' @param sinkmethod  options are planchon.darboux.2001" (default), "xxl.wang.liu.2006" and "wang.liu.2006"
#' @param minslope    minimum slope to impose on filled cells to force drainage. Default = 0.1
#' @param saga.env    SAGA environment object.  Default is to let saga find it on its own.
#' @return dem_ns filled dem as a raster object 
#' ch_saga_fillsinks()

ch_saga_fillsinks <- function(dem_raw, saga_wd, 
                              sinkmethod = "planchon.darboux.2001", 
                              minslope = 0.1,
                              saga.env = RSAGA::rsaga.env()) {
  #
  library(RSAGA)
  library(raster)
  # error trap - saga_wd does not exist
  if (!dir.exists(saga_wd)) {
    print("saga_wd does not exist")
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
  crs(dem_ns) <- crs(dem_raw)
  return(dem_ns)
} # end function ch_saga_fillsinks

