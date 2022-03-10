#' Generate KML file to check delineation in Google Earth
#'
#' @param fn_kml Name of the KML file to be crearted
#' @param folder_name name of the KML folder.
#' @param contours \pkg{sp} object of contours.
#' @param catchments \pkg{sp} object of catchments.
#' @param channels \pkg{sp} object of channels.
#' @param pp \pkg{sp} object of pour points.
#' @param pp_labels A vector of names for the pour points. If not specified, the 
#' pour points will be numbered.
#' @author Dan Moore and Kevin Shook
#' @importFrom sf st_cast st_crs
#' @importFrom plotKML kml_open kml_layer kml_close
#' @return \code{TRUE}. A kml file is also created.
#' @export
#'
#' @examples
#' \donttest{
#' # Not tested automatically as requires installation of Whitebox
#' library(raster)
#' test_raster <- ch_volcano_raster()
#' dem_raster_file <- tempfile(fileext = c(".tif"))
#' no_sink_raster_file <- tempfile("no_sinks", fileext = c(".tif"))
#' 
#' # write test raster to file
#' writeRaster(test_raster, dem_raster_file, format = "GTiff")
#' 
#' # remove sinks
#' removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, method = "fill")
#' 
#' # get flow accumulations
#' flow_acc_file <- tempfile("flow_acc", fileext = c(".tif"))
#' flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)
#' 
#' # get pour points
#' pourpoint_file <- tempfile("volcano_pourpoints", fileext = c(".shp"))
#' pourpoints <- ch_volcano_pourpoints(pourpoint_file)
#' snapped_pourpoint_file <- tempfile("snapped_pourpoints", fileext = c(".shp"))
#' snapped_pourpoints <- ch_wbt_pourpoints(pourpoints, flow_acc_file, pourpoint_file,
#' snapped_pourpoint_file, snap_dist = 10)
#' 
#' # get flow directions
#' flow_dir_file <- tempfile("flow_dir", fileext = ".tif")
#' flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
#' fn_catchment_ras <- tempfile("catchment", fileext = ".tif")
#' fn_catchment_vec <- tempfile("catchment", fileext = ".shp")
#' 
#' # generate contours
#' contours <- ch_contours(test_raster)
#' catchments <- ch_wbt_catchment(snapped_pourpoint_file, flow_dir_file, 
#' fn_catchment_ras, fn_catchment_vec)
#' 
#' # get channels
#' channel_raster_file <- tempfile("channels", fileext = c(".tif"))
#' channel_vector_file <- tempfile("channels", fileext = c(".shp"))
#' channels <- ch_wbt_channels(flow_acc_file, flow_dir_file, channel_raster_file,
#' channel_vector_file, 1)
#' fn_kml <- tempfile("volcano", fileext = ".KML")
#' 
#' # create KML files, suppressing warnings caused by other functions
#' suppressWarnings(ch_kml_checkcatchment(fn_kml, "test", contours, catchments, 
#' channels, pourpoints))
#' }
#' 
ch_kml_checkcatchment <- function(fn_kml, folder_name = "",
                                  contours, catchments, channels, 
                                  pp, pp_labels = NULL) {

  catchments <- st_cast(catchments, 'MULTILINESTRING') %>%
    st_cast('LINESTRING')
  # convert to contours to linestring so all contour segments get plotted
  contours <- st_cast(contours, 'LINESTRING')
  # generate a vector of labels for pour points
  np <- nrow(pp)
  if (!is.null(pp_labels)) {
    labels <- pp_labels
  } else {
    labels <- rep("", np)
  }
  # create layers
  kml_open(file.name = fn_kml, folder.name = folder_name)
  kml_layer(as_Spatial(contours), colour = "#aaaa7f",
                     subfolder.name = "Contours")
  kml_layer(as_Spatial(channels), colour = "lightblue",
                     subfolder.name = "Channels")
  kml_layer(as_Spatial(catchments), colour = "yellow",
                     #outline = TRUE, alpha = 0.1,
                     subfolder.name = "Catchments")
  kml_layer(as_Spatial(pp), 
                     colour = "red", 
                     shape = paste0("https://maps.google.com/mapfiles/kml/shapes",
                                    "/placemark_circle_highlight.png"),
                     subfolder.name = "Catchment outlets",
                     points_name = labels)
  kml_close(file.name = fn_kml)
  
  return(TRUE)
}
