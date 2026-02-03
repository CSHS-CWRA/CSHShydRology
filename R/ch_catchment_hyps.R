#' Catchment hypsometry
#' 
#' @description Finds the hypsometric curve, which is the total fraction of
#' the area below vs. elevation, for a given basin. 
#' 
#' @details
#' The elevations may be passed as a vector of elevations, or of elevation quantiles, or as minimum
#' and maximum elevations and the number of elevation intervals. A plot of the 
#' curve may also be created.
#' 
#' @param catchment A \pkg{sf} object containing the catchment divide.
#' @param dem A \pkg{raster} object of the Digital Elevation Model.
#' @param z_levels Vector of elevation levels for the hypsometry. If specified, 
#' then no other elevation parameters are required. Default is \code{NULL}.
#' @param n_levels If specified, sets number of elevation intervals. 
#' Can be used with \code{zmin} and \code{zmax}. Default is \code{NULL}.
#' @param zmin Minimum elevation for hypsometry. If not specified, minimum
#' catchment elevation is used. Default is \code{NULL}.
#' @param zmax Maximum elevation for hypsometry. If not specified, maximum
#' catchment elevation is used. Default is \code{NULL}.
#' @param quantiles Vector of elevation quantiles. Default is \code{NULL}.
#' @param hypso_plot if \code{TRUE} the hypsometric curve is plotted. Default is
#' \code{NULL}.
#' @param z_units Elevation units for plot. Default is \option{m}. 
#' @param col Colour for plot. Default is \option{red}.
#' @param type Type of plot. Defailt is \option{o} (lines with overplotted
#' points).
#' @param xlab Plot x-axis label.
#' @param ylab Plot y-axis label.
#' @param add_grid If \code{TRUE}, a grid is added to the plot. Default is 
#' \code{FALSE}
#' @param ... Other parameters for the graph
#'
#' @importFrom sf as_Spatial
#' @importFrom raster mask minValue maxValue 
#' @return Returns a data frame of elevations and catchment fractions below.
#' @author Dan Moore Kevin Shook
#' @seealso \code{\link{ch_get_url_data}}  
#' @export
#'
#' @examples \donttest{
#' # Note: example not tested automatically as it is very slow to execute due to the downloading
#' library(raster)
#' library(magrittr)
#' # change the following line to specify a directory to hold the data
#' dir_name <- tempdir(check = FALSE)
#' # create directory to store data sets
#' if (!dir.exists(dir_name)) {
#'   dir.create(dir_name, recursive = TRUE)
#' }
#' # get 25-m dem
#' dem_fn <- file.path(dir_name, "gs_dem25.tif")
#' dem_url <- "https://zenodo.org/record/4781469/files/gs_dem25.tif"
#' dem_upc <- ch_get_url_data(dem_url, dem_fn, stop_on_error = FALSE)
#' dem_upc_type <- typeof(dem_upc)
#' if (dem_upc_type != "character") {
#' # get catchment boundaries
#'   cb_fn <- file.path(dir_name, "gs_catchments.GeoJSON")
#'   cb_url <- "https://zenodo.org/record/4781469/files/gs_catchments.GeoJSON"
#'   cb <- ch_get_url_data(cb_url, cb_fn, stop_on_error = FALSE)
#'   cb_type <- typeof(cb)
#'   if (cb_type != "character") {
#'   #  quick check plot - all catchments
#'     raster::plot(dem_upc)
#'     plot(cb, add = TRUE, col = NA)
#' 
#'     # subset 240 catchment
#'     cb_240 <- cb %>% dplyr::filter(wsc_name == "240")
#'     plot(cb_240, col = NA)
#' 
#'     ## test function 
#' 
#'     # test different combinations of arguments
#'     ch_catchment_hyps(cb_240, dem_upc, quantiles = seq(0, 1, 0.1))
#'     ch_catchment_hyps(cb_240, dem_upc, z_levels = seq(1600, 2050, 50))
#'     ch_catchment_hyps(cb_240, dem_upc, n_levels = 6)
#'     ch_catchment_hyps(cb_240, dem_upc)
#'     ch_catchment_hyps(cb_240, dem_upc, zmin = 1600, zmax = 2050)
#'     ch_catchment_hyps(cb_240, dem_upc, zmin = 1600, zmax = 2050, n_levels = 6)
#' 
#'     # generate a graph
#'     ch_catchment_hyps(cb_240, dem_upc, hypso_plot = TRUE)
#'     ch_catchment_hyps(cb_240, dem_upc, hypso_plot = TRUE, 
#'               col = "blue", type = "l", ylim = c(1500, 2200))
#'     ch_catchment_hyps(cb_240, dem_upc, hypso_plot = TRUE, 
#'               add_grid = TRUE, quantiles = seq(0, 1, 0.1))
#'     ch_catchment_hyps(cb_240, dem_upc, hypso_plot = TRUE,
#'               ylab = expression("z ("*10^{-3} ~ "km)"))
#' 
#'     # extract specific quantiles (e.g., median and 90%)
#'     ch_catchment_hyps(cb_240, dem_upc, quantiles = c(0.5,0.9))
#'   }
#'  }
#' }

ch_catchment_hyps <- function(catchment, dem,
                          z_levels = NULL, n_levels = 10,
                          zmin = NULL, zmax = NULL, 
                          quantiles = NULL,
                          hypso_plot = FALSE, z_units = "m", 
                          col = "red", type = "o", 
                          xlab = "Fraction of catchment below given elevation",
                          ylab = paste0("Elevation (", z_units, ")"), 
                          add_grid = FALSE, ...) {
  # need to add error traps for incorrect values for
  # catchment and dem
  catchment_sp <- as_Spatial(catchment)
  dem_masked <- raster::mask(dem, catchment_sp)
  if (is.null(quantiles)) {
    if (is.null(z_levels)) {
      if (is.null(zmin)) zmin <- raster::minValue(dem_masked)
      if (is.null(zmax)) zmax <- raster::maxValue(dem_masked)    
      z_levels <- seq(zmin, zmax, length.out = n_levels)
    }
    # there may be a more direct way, but this works
    z_hist <- raster::hist(dem_masked, plot = FALSE, breaks = z_levels)
    nz <- sum(z_hist$counts)
    qz <- c(0, cumsum(z_hist$counts)/nz)
    out_df <- data.frame(z = z_levels, qz)
  } else {
    zq <- raster::quantile(dem_masked, probs = quantiles)
    out_df <- data.frame(z = zq, qz = quantiles)
  }
  if (hypso_plot) {
    plot(out_df$qz, out_df$z,
         col = col, xlab = xlab, ylab = ylab, type = type, ...
    )
    if (add_grid) grid()
  }
  return(out_df)
}
