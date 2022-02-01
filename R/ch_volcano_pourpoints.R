#' Creates a sample file of pour points
#' 
#' @description Creates a file of pour points for the \code{volcano} DEM
#' @param pp_shp Name for shapefile to hold pour points
#'
#' @return Returns \code{NULL}. Writes pour points to specified file.
#' @export
#' @importFrom dplyr mutate
#' @importFrom sf st_as_sf st_set_crs st_write
#' @author Dan Moore
#' @seealso \code{\link{ch_volcano_raster}} \code{\link{ch_wbt_pourpoints}} 
#'
#' @examples
ch_volcano_pourpoints <- function(pp_shp) {
  
  if(missing(pp_shp)) {
    stop("File for pour points must be specified")
  }

  outlet_sf <- data.frame(x = c(300570, 300644),
                          y = c(5916757, 5916557)) %>%
    mutate(test_label = c("test_1", "test_2")) %>%
    st_as_sf(coords = c("x", "y")) %>%
    st_set_crs(32760)
  st_write(outlet_sf, pp_shp, delete_dsn = TRUE)
  return(NULL)
}