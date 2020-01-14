#' Create Catchment Polygons
#'
#' generates a grid of contributing area for each dem grid cell
#'
#' @param dem Raster object of your dem in the desired projection - should have had sinks removed
#' @param carea raster object containing contributing areas (default none provided)
#' @param carea_flag if carea = NULL, 0 = create carea from dem; 1 = read in carea.sdat
#' @param sagawd name of working directory
#' @param outlet sf point object with coordinates of catchment outlets in the same projection as the dem
#' @param outlet_label character vector of labels; if "NULL", numbers are assigned
#' @param buff_size numeric; the buffer radius (m) around catchment outlet to find location on digital stream network.
#' @param saga.env SAGA environment object.  Default is to let saga find it on its own.
#' @import RSAGA raster sf
#' 
#' @return Returns an sf object containing catchment polygons
#' @examples ch_saga_catchment()

ch_saga_catchment <- function(dem, saga_wd, outlet,
                              carea = NULL, carea_flag = 0,
                              outlet_label = NULL,
                              buffsize = 100,
                              saga.env = RSAGA::rsaga.env()) {


  # error trap - saga_wd does not exist
  if (!dir.exists(saga_wd)) {
    print("saga_wd does not exist")
    return(NA)
  }
  # store the dem in the working directory
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
    # read the contributing area grid into a raster object
    carea <- raster::raster(paste0(saga_wd, '/carea.sdat'))
  }
  # extract coordinates into a matrix
  xy <- st_coordinates(outlet)
  nc <- nrow(outlet)
  polygon_sfc <- sf::st_sfc()
  # print catchment area with units
  if (is.null(outlet_label)) {
    labels <- as.character(1:nc)
  } else {
    labels <- outlet_label
  }
  for (i in 1:nc) {
    # check that outlet lies within dem bbox
    bb <- bbox(dem)
    if (xy[i, 1] < bb[1, 1] | xy[i, 1] > bb[1,2] |
        xy[i, 2] < bb[2, 1] | xy[i, 2] > bb[2,2]) {
      print(cat("outlet", i, "does not lie within dem"))
      polygon_sfc[i] <- NULL
    } else {
      # extract a window around around the outlet
      buffer <- raster::extract(carea, outlet[i,], buffer = buffsize,
                                cellnumbers = TRUE)[[1]] %>%
        as.data.frame
      # find the location of the maximum catchment area on the grid, given as the id from the raster
      snap_id <- buffer$cell[which.max(buffer$value)]
      # get the xy coordinates at that max location, which is now going to be the location of the outlet
      snap_xy <- raster::xyFromCell(carea, snap_id)
      # make catchment as grid
      RSAGA::rsaga.geoprocessor(lib = 'ta_hydrology', 4,
                                param = list(TARGET_PT_X = snap_xy[1,1],
                                             TARGET_PT_Y = snap_xy[1,2],
                                             ELEVATION = paste0(saga_wd, '/dem.sgrd'),
                                             AREA = paste0(saga_wd, '/bounds.sgrd'),
                                             METHOD = 0),
                                env = saga.env)
      # convert grid to vector shapefile
      RSAGA::rsaga.geoprocessor(lib = 'shapes_grid', 6,
                                param = list(GRID = paste0(saga_wd, '/bounds.sgrd'),
                                             POLYGONS = paste0(saga_wd, '/bounds.shp'),
                                             CLASS_ALL = 0,
                                             CLASS_ID = 100,
                                             SPLIT = 0),
                                env = saga.env)
      # read polygon shapefile and save in sf object
      polygon_sfc[i] <- st_geometry(sf::st_read(paste0(saga_wd, '/bounds.shp')))
    }
  }
  # create data frame
  catchment_sf <- data.frame(label = outlet_label,
                             outlet_x = xy[, 1], outlet_y = xy[, 2]) %>%
    cbind(polygon_sfc) %>%
    st_as_sf()
  # set crs
  sf::st_crs(catchment_sf) <- raster::crs(dem)@projargs
  return(catchment_sf)
} # end function ch_saga_catchment
