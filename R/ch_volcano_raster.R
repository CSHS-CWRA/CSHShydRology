#' Create Test Raster
#'
#' @description Creates a \pkg{raster} object of land surface elevations, as
#' used to test/demonstrate many functions requiring a digital elevation model 
#' (DEM. No arguments are required as the DEM is created from the \pkg{base} 
#' \code{volcano} matrix of elevations.
#' @export 
#' @return Returns a raster object of land surface elevations. 
#' @author Dan Moore and Kevin Shook
#' @importFrom raster raster t
#' @seealso \code{\link{ch_saga_fillsinks}} \code{\link{ch_saga_carea}}
#' @examples
#' test_raster <- ch_volcano_raster()
ch_volcano_raster <- function() {
  vol_mat <- volcano
  nr <- nrow(vol_mat)
  nc <- ncol(vol_mat)
  dx <- 10
  xmin <- 5
  xmax <- xmin + (nc - 1)*dx
  ymin <- 5
  ymax <- ymin + (nr - 1)*dx
  x <- rep(seq(xmax, xmin, -dx), each = nr)
  y <- rep(seq(ymin, ymax, dx), times = nc)
  vol_ras <- data.frame(x, y, z = as.numeric(vol_mat)) %>%
    raster::rasterFromXYZ()
  return(vol_ras)
}
