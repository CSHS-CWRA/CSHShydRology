#' Creates a sample file of pour points
#' 
#' @description Creates a file of pour points for the \code{volcano} DEM. The 
#' pour points define the outlets of sub-basins. These pour points are used
#' by examples within other functions.
#' @param pp_shp Name for shapefile to hold pour points
#'
#' @return Returns an \pkg{sf} object containing 2 pour points for the 
#' \code{volcano} DEM. The pour points are also written to the specified file.
#' @export
#' @importFrom dplyr mutate
#' @importFrom sf st_as_sf st_set_crs st_write
#' @author Dan Moore and Kevin Shook
#' @seealso \code{\link{ch_volcano_raster}} \code{\link{ch_wbt_pourpoints}} 
#'
#' @examples
#' pourpoint_file <- tempfile("volcano_pourpoints", fileext = c(".shp"))
#' pourpoints <- ch_volcano_pourpoints(pourpoint_file)
#' plot(pourpoints)
ch_volcano_pourpoints <- function(pp_shp) {
  
  if(missing(pp_shp)) {
    stop("File for pour points must be specified")
  }

  outlet_sf <- data.frame(x = c(300570, 300644),
                          y = c(5916757, 5916557)) %>%
    mutate(test_label = c("test_1", "test_2")) %>%
    st_as_sf(coords = c("x", "y")) %>%
    st_set_crs(32760)
  if (file.exists(pp_shp))
    st_write(outlet_sf, pp_shp, delete_dsn = TRUE)
  else
    st_write(outlet_sf, pp_shp, delete_dsn = FALSE)
  return(outlet_sf)
}