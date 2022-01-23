#' Generate kml file to check delineation in Google Earth
#'
#' @param file 
#' @param folder_name 
#' @param contours 
#' @param catchments 
#' @param channels 
#' @param pp 
#' @param pp_labels 
#'
#' @return
#' @export
#'
#' @examples
ch_kml_checkcatchment <- function(file, folder_name = "",
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
  plotKML::kml_open(file.name = fn_kml, folder.name = folder_name)
  plotKML::kml_layer(as_Spatial(contours), colour = "#aaaa7f",
                     subfolder.name = "Contours")
  plotKML::kml_layer(as_Spatial(channels), colour = "lightblue",
                     subfolder.name = "Channels")
  plotKML::kml_layer(as_Spatial(catchments), colour = "yellow",
                     #outline = TRUE, alpha = 0.1,
                     subfolder.name = "Catchments")
  plotKML::kml_layer(as_Spatial(pp), 
                     colour = "red", 
                     shape = paste0("https://maps.google.com/mapfiles/kml/shapes",
                                    "/placemark_circle_highlight.png"),
                     subfolder.name = "Catchment outlets",
                     points_name = labels)
  plotKML::kml_close(file.name = fn_kml)
}
