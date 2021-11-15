#' Create Test Raster
#'
#' @description Creates a \pkg{raster} object of land surface elevations, as
#' used to test/demonstrated the {ch_saga} functions. No arguments are required
#' as the value is created from the \pkg{base} \code{volcano} matrix of elevations.
#' @return
#' @export Returns a \pkg{raster} of land surface elevations. 
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
  vol_ras <- raster(volcano, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax)
  return(vol_ras)
}
