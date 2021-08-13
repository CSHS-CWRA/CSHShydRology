#' Check Catchments
#'
#' This function generates a simple map to allow a visual assessment of the catchment 
#' boundaries relative to the elevation contours. It also generates a table summarizing the catchments, 
#' including the coordinates of the outlet point and the catchment area.
#'
#' @param dem Raster. DEM that catchment was generated from
#' @param catchment sf. catchment polygon
#' @param outlet sf. location of catchment outlet
#' @param outlet_label string. Label for outlet
#' @return 
#' \item{area_df}{Generates contour map with catchment polygon and outlet check}
#' 
#' 
#' @author Dan Moore <dan.moore@ubc.ca>
#' @seealso \code{\link{ch_saga_fillsinks}} to fill sinks instead of removing
#' 
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
#' 
#' # check catchments
#' ch_checkcatchment(filled_dem, catchments, stns)
#' }
#' 
#' @importFrom sf st_bbox st_area
#' @importFrom ggplot2 ggplot geom_sf coord_sf theme_bw 
#' @importFrom ggspatial annotation_north_arrow north_arrow_fancy_orienteering annotation_scale 
#' @importFrom dplyr mutate 
#' @importFrom grid unit
#' @export
ch_checkcatchment <- function(dem, catchment, outlet, outlet_label = NULL) {

  # require some error checking here on inputs
  
  # create contours and get bounding box to set map limits
  contours <- ch_contours(dem)
  bb <- st_bbox(contours)
  # generate map
  check_map <- ggplot(data = contours) +
    geom_sf(data = contours, color = "grey") +
    geom_sf(data = outlet, pch = 21, bg = "blue") +
    geom_sf(data = st_geometry(catchment), fill = NA, color = "red") +
    annotation_north_arrow(style = north_arrow_fancy_orienteering,
                                      location = "tr",
                                      pad_x = unit(4, "mm"),
                                      pad_y = unit(6.5, "mm")) +
    annotation_scale() +
    coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4])) +
    theme_bw()
  
  print(check_map)
  nc <- nrow(catchment)
  
  # print catchment area with units
  if (is.null(outlet_label)) {
    labels <- as.character(1:nc)
  } else {
    labels <- outlet_label
  }
  
  area <- st_area(catchment)
  units <- rep(paste0(attr(area, "units")$numerator[1], "^2"), length(area))
  value <- round(as.numeric(area))
  area_df <- outlet %>%
    mutate(label = labels, area = value, area_units = units)
  
  return(area_df)
}

