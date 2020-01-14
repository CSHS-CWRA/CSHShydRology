#' Create Catchment Channels
#'
#' determines channel layer spatial files
#'
#' @param dem Raster object of your dem in the desired projection - should have had sinks removed
#' @param carea raster object containing contributing areas (default none provided)
#' @param carea_flag if carea = NULL, 0 = create carea from dem; 1 = read in carea.sdat
#' @param saga_wd name of working directory
#' @param out_shp if TRUE, return channel network as sf object
#' @param out_ntwrk if TRUE, return specified object as a raster
#' @param out_route if TRUE, return specified object as a raster
#' @param initvalue parameters for SAGA function
#' @param initmethod parameters for SAGA function
#' @param divcells parameters for SAGA function
#' @param minlen parameters for SAGA function
#' @param saga.env    SAGA environment object.  Default is to let saga find it on its own.
#' @return out_list list containing (as specified) channels (shp), ntwrk (raster) and route (raster)
#' ch_saga_channels()

ch_saga_channels <- function(dem, saga_wd, carea = NULL, carea_flag = 0,
                             out_shp = TRUE,
                             out_ntwrk = FALSE, out_route = FALSE,
                             initmethod = 2, initvalue = 0,
                             divcells = 5, minlen = 10,
                             saga.env = RSAGA::rsaga.env()) {

  library(RSAGA)
  library(raster)
  library(sf)

  # error trap - saga_wd does not exist
  if (!dir.exists(saga_wd)) {
    print("saga_wd does not exist")
    return(NA)
  }
  # make a temporary directory within the working directory and store the dem there
  raster::writeRaster(dem, paste0(saga_wd, "/dem.sdat"), format = "SAGA",
                      NAflag = -9999, overwrite = TRUE)
  # if a contributing area raster was not included, create one
  if (is.null(carea)) {
    if (carea_flag == 0) {
      # generate contributing area grid - use all defaults
      RSAGA::rsaga.topdown.processing(in.dem = paste0(saga_wd, "/dem.sgrd"),
                                      out.carea = paste0(saga_wd, "/carea.sgrd"),
                                      env = saga.env)
    }
    carea <- raster::raster(paste0(saga_wd, '/carea.sdat'))
  }
  # generate channel network
  RSAGA::rsaga.geoprocessor(lib = 'ta_channels', 0,
                            param = list(ELEVATION = paste0(saga_wd, '/dem_ns.sgrd'),
                                         INIT_GRID = paste0(saga_wd, '/carea.sgrd'),
                                         INIT_METHOD = initmethod,
                                         INIT_VALUE = initvalue,
                                         DIV_CELLS = divcells,
                                         MINLEN = minlen,
                                         CHNLNTWRK = paste0(saga_wd, '/ntwrk.sdat'),
                                         CHNLROUTE = paste0(saga_wd, '/route.sdat'),
                                         SHAPES = paste0(saga_wd, '/channels.shp')),
                            env = saga.env)
  out_list <- list(channels = NULL, ntwrk = NULL, route = NULL)
  if (out_shp) {
    channels_sf <- st_geometry(sf::st_read(paste0(saga_wd, '/channels.shp')))
    sf::st_crs(channels_sf) <- raster::crs(dem)@projargs
    out_list$channels <- channels_sf
  }
  if (out_ntwrk) {
    ntwrk <- raster::raster(paste0(saga_wd, '/ntwrk.sdat'))
    crs(ntwrk) <- crs(dem)
    out_list$ntwrk <- ntwrk
  }
  if (out_route) {
    route <- raster::raster(paste0(saga_wd, '/route.sdat'))
    crs(route) <- crs(dem)
    out_list$route <- route
  }
  return(out_list)
} # end ch_channels
