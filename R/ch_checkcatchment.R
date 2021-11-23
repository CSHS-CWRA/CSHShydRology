#' Check Catchments
#'
#' @description
#' Generates a simple map to allow a visual assessment of the catchment 
#' boundaries relative to the elevation contours. 
#' 
#' @details 
#' Also generates a table summarizing the catchments, 
#' including the coordinates of the outlet point and the catchment area.
#'
#' @param dem raster DEM that catchments were generated from
#' @param catchment catchment polygon (sf object)
#' @param outlet location of catchment outlet (sf object)
#' @param outlet_label character label for outlet
#' @param main_label main label for catchment
#' @param bbox_type type of bounding box. If \option{catchment}, then
#' the contours are bounded by the catchment, otherwise they are plotted
#' to the extent of the DEM
#' @return 
#' \item{area_df}{Generates contour map with catchment polygon and outlet check}
#' 
#' @author Dan Moore
#' @seealso \code{\link{ch_saga_fillsinks}} to fill sinks instead of removing
#' 
#' @examples
#' 
#' \donttest{
#' # note: example not tested in package compilation
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
#' catchments <- ch_saga_catchment(dem=filled_dem, saga_wd=saga_wd, 
#' outlet=stns, carea=carea)
#' 
#' # check catchments
#' ch_checkcatchment(filled_dem, catchments, stns)
#' }
#' 
#' @importFrom sf st_bbox st_area
#' @importFrom ggplot2 ggplot geom_sf coord_sf theme_bw labs
#' @importFrom ggspatial annotation_north_arrow north_arrow_fancy_orienteering annotation_scale 
#' @importFrom dplyr mutate 
#' @importFrom grid unit
#' @export

ch_checkcatchment <- function(dem, catchment, outlet, outlet_label = NULL,
                                main_label = "", bbox_type = "catchment") {
  # check inputs
  if (missing(catchment)) {
    stop("ch_checkcatchment requires sf catchment polygons to plot")
  }
  if (missing(dem)) {
    stop("ch_checkcatchment requires a raster dem to plot")
  }
  if (missing(outlet)) {
    stop("ch_checkcatchment requires an sf outlet to plot")
  }
  
  # create contours and get bounding box to set map limits
  # create contours 
  contours = ch_contours(dem)
  # generate bounding box
  if (bbox_type == "catchment") {
    bb = sf::st_bbox(catchment)
  } else {
    bb <- sf::st_bbox(contours)
  }
  # generate map
  check_map <- ggplot2::ggplot() +
    geom_sf(data = contours, color = "grey") +
    geom_sf(data = outlet, pch = 21, bg = "blue") +
    geom_sf(data = sf::st_geometry(catchment), fill = NA, color = "red") +
    ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering, 
                                      location = "tr",
                                      pad_x = unit(4, "mm"), 
                                      pad_y = unit(6.5, "mm")) +
    ggspatial::annotation_scale() +
    coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4])) +
    labs(title = main_label) +
    theme_bw()
  print(check_map)
  nc <- nrow(outlet)
  # print catchment area with units
  if (is.null(outlet_label)) {
    labels <- as.character(1:nc) 
  } else {
    labels <- outlet_label
  }
  area <- sf::st_area(catchment)
  units <- rep(paste0(attr(area, "units")$numerator[1], "^2"), length(area))
  value <- round(as.numeric(area))
  area_df <- outlet %>%
    mutate(label = labels, area = value, units = units)

  return(area_df)
}

