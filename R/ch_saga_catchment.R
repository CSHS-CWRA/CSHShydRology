#' Create Catchment Polygons
#'
#' This function generates catchment boundaries for a set of specified points of interest along a stream network. The output is a **sf** object containing the polygons. 
#' 
#' The function has two gridded inputs: a DEM (with sinks removed as appropriate) and a grid of contributing areas. Other inputs include `outlet`, a **sf** object containing the coordinates of the catchment outlets, and `outlet_label`, an optional vector of character strings that serve as labels or descriptions of the outlet points.
#' 
#' The DEM is input as a raster object via the `dem` argument.
#' 
#' There are three ways to provide the contributing area grid, which can be accomplished using the `carea` and `carea_flag` arguments, as summarized below. 
#' 
#' The first option (`carea = NULL` and `carea_flag = 0`) would be appropriate if you have not generated a contributing area grid prior to application of `ch_saga_catchment`. If you use this option, the contributing area grid is generated using the standard SAGA defaults, with no options for the user to change them.
#' 
#' The second option (`carea = NULL` and `carea_flag = 1`) would be appropriate if you have run the `ch_saga_carea` function and have not cleared the working directory.
#' 
#' The third option would be appropriate if you have generated a contributing area grid as a raster object using either `ch_saga_carea` or another function.
#' 
#' The second and third options are particularly useful if the user either (a) wants to use a different approach than the SAGA defaults to generate the contributing area grid or (b) wants to use the contributing area grid in other analyses (e.g., computing a topographic wetness index).
#' 
#' Note that only one of the `carea` and `carea_flag` arguments need be specified.
#'
#' @param dem Raster object of your dem in the desired projection - should have had sinks removed
#' @param carea raster object containing contributing areas (default none provided)
#' @param carea_flag if carea = NULL, 0 = create carea from dem; 1 = read in carea.sdat
#' @param saga_wd name of working directory
#' @param outlet sf point object with coordinates of catchment outlets in the same projection as the dem
#' @param outlet_label character vector of labels; if "NULL", numbers are assigned
#' @param buffsize numeric; the buffer radius (m) around catchment outlet to find location on digital stream network.
#' @param saga.env SAGA environment object.  Default is to let saga find it on its own.
#' @return  
#' \item{catchment_sf}{Returns an sf object containing catchment polygons}
#' 
#' @importFrom RSAGA rsaga.topdown.processing rsaga.geoprocessor 
#' @importFrom raster writeRaster raster crs extract 
#' @importFrom sf st_coordinates st_geometry st_sfc st_as_sf
#' @importFrom sp bbox
#' 
#' @author Dan Moore <dan.moore@ubc.ca>
#' @seealso \code{\link{ch_saga_fillsinks}} to fill sinks instead of removing
#' @export
#' @examples
#' 
#' \dontrun{
#' # note: example not run in package compilation
#' # - requires creating and accessing a temporary directory
#' # - requires downloading spatial data from Zenodo repository
#' # - requires multiple potentially lengthy GIS operations
#' 
#' # create saga wd using base::tempdir()
#' saga_wd <- tempdir()
#' 
#' # download LiDAR DEM for 240 and 241 creek
#' ff <- "gs_be240.tif"
#' ra_fn <- file.path(saga_wd, ff)
#' ra_url <- sprintf("https://zenodo.org/record/4781469/files/%s",ff)
#' dem <- ch_get_url_data(ra_url, ra_fn)
#' 
#' # fill sinks
#' filled_dem <-  ch_saga_fillsinks(dem_raw=dem, saga_wd=saga_wd)
#' 
#' # download station locations for 240, 241 (use as catchment outlets)
#' ff <- "gs_weirs.GeoJSON"
#' gs_fn <- file.path(saga_wd, ff)
#' gs_url <- sprintf("https://zenodo.org/record/4781469/files/%s",ff)
#' stns <- ch_get_url_data(gs_url, gs_fn)[1:2,]
#' 
#' # determine contributing area raster using filled_dem
#' carea <- ch_saga_carea(filled_dem, saga_wd)
#' 
#' # run catchment delineation
#' catchments <-  ch_saga_catchment(dem=filled_dem, saga_wd=saga_wd, outlet=stns, carea=carea)
#' }
#' 
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
  catchment_sf <- data.frame(label = labels,
                             outlet_x = xy[, 1], outlet_y = xy[, 2]) %>%
    cbind(polygon_sfc) %>%
    st_as_sf()
  # set crs
  sf::st_crs(catchment_sf) <- raster::crs(dem)@projargs
  return(catchment_sf)
}