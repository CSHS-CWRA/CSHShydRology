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
#' @param dem raster DEM that catchments were generated from.
#' @param catchment Catchment polygon (sf object).
#' @param outlet Location of catchment outlet (sf object).
#' @param outlet_label Character label for outlet.
#' @param main_label Main label for catchment plot.
#' @param bbox_type type of bounding box. If \option{catchment}, then
#' the contours are bounded by the catchment, otherwise they are plotted 
#' to the extent of the DEM
#' @param channel_vec Vectors of the channels will be plotted if specified.
#' @param cb_colour Colour for catchment outline. Default is "red".
#' @param pp_colour Colour for catchment pour points. Default is "red".
#' @param channel_colour Colour for channel. Default is "blue".
#' @param contour_colour Colour for contours Default is "grey".
#' @param plot_na If \code{TRUE} (the default) a north arrow is added to the plot.
#' @param plot_scale If \code{TRUE} (the default) a scale bar is added to the plot.
#' @param na_location Location for the north arrow. Default is \option{tr}, i.e. top-right.
#' @param scale_location Location for the scale bar. Default is \option{bl}, i.e. bottom-left.
#' 
#' @return \code{TRUE}. A map of the catchments is also plotted and 
#' the catchment parameters are printed.
#' 
#' @author Dan Moore and Kevin Shook
#' @seealso \code{\link{ch_checkchannels}} 
#' @importFrom sf st_bbox st_area st_crs st_geometry
#' @importFrom ggplot2 ggplot geom_sf coord_sf theme_bw labs
#' @importFrom ggspatial annotation_north_arrow north_arrow_fancy_orienteering annotation_scale 
#' @importFrom dplyr mutate 
#' @importFrom grid unit
#' @export
#' @examples
#' \donttest{
#' # Not tested automatically as requires installation of Whitebox
#' library(raster)
#' test_raster <- ch_volcano_raster()
#' dem_raster_file <- tempfile(fileext = ".tif")
#' no_sink_raster_file <- tempfile("no_sinks", fileext = ".tif")
#' 
#' # write test raster to file
#' writeRaster(test_raster, dem_raster_file, format = "GTiff")
#' 
#' # remove sinks
#' removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, 
#' method = "fill")
#' 
#' # get flow accumulations
#' flow_acc_file <- tempfile("flow_acc", fileext = ".tif")
#' flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)
#' 
#' # get pour points
#' pourpoint_file <- tempfile("volcano_pourpoints", fileext = ".shp")
#' pourpoints <- ch_volcano_pourpoints(pourpoint_file)
#' snapped_pourpoint_file <- tempfile("snapped_pourpoints", fileext = ".shp")
#' snapped_pourpoints <- ch_wbt_pourpoints(pourpoints, flow_acc_file, pourpoint_file,
#' snapped_pourpoint_file, snap_dist = 10)
#' 
#' # get flow directions
#' flow_dir_file <- tempfile("flow_dir", fileext = ".tif")
#' flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
#' fn_catchment_ras <- tempfile("catchment", fileext = ".tif")
#' fn_catchment_vec <- tempfile("catchment", fileext = ".shp")
#' catchments <- ch_wbt_catchment(snapped_pourpoint_file, flow_dir_file, 
#' fn_catchment_ras, fn_catchment_vec)
#' 
#' # check results
#' ch_checkcatchment(test_raster, catchments, snapped_pourpoints)
#' }
ch_checkcatchment <- function(dem, catchment, outlet, outlet_label = NULL,
                                main_label = "", bbox_type = "catchment",
                                channel_vec = NULL, 
                                cb_colour = "red", pp_colour = "red", 
                                channel_colour = "blue", contour_colour = "grey",
                                plot_na = TRUE, plot_scale = TRUE,
                                na_location = "tr", scale_location = "bl") {
    
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
    # create contours 
    contours = ch_contours(dem)
    # generate bounding box
    if (bbox_type == "catchment") {
      bb = st_bbox(catchment)
    } else {
      bb <- st_bbox(contours)
    }
    # generate map
    check_map <- ggplot2::ggplot() +
      geom_sf(data = contours, color = contour_colour)
    if (!is.null(channel_vec)) {
      check_map <- check_map + 
        geom_sf(data = channel_vec, col = channel_colour) +
        coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), 
                 datum = st_crs(catchment))
    }
    check_map <- check_map +
      geom_sf(data = outlet, pch = 21, bg = pp_colour) +
      geom_sf(data = st_geometry(catchment), fill = NA, color = cb_colour) +
      coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]),
               datum = st_crs(catchment)) +
      labs(title = main_label) +
      theme_bw()
    if (plot_na) {
      check_map <- check_map +
       annotation_north_arrow(style = north_arrow_fancy_orienteering, 
                                          location = na_location,
                                          pad_x = unit(4, "mm"), 
                                          pad_y = unit(6.5, "mm"))
    }
    if (plot_scale) {
      check_map <- check_map +
        annotation_scale(location = scale_location)
    }
    print(check_map)
    nc <- nrow(outlet)
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
      mutate(label = labels, area = value, units = units)

    return(TRUE)
  } 

