#' Spatial Hydrology functions
#' @name SpatialHydrology-functions
#' @description These functions perform spatial analyses important in hydrology
#' \describe{
#'  \item{ch_saga_carea}{Generates a grid of contributing area for each grid cell}
#'  \item{ch_saga_removesinks}{Removes sinks from a DEM by deepening drainage network}
#'  \item{ch_saga_fillsinks}{Removes sinks from a DEM by filling them}
#'  \item{ch_saga_catchment}{Generates catchment boundaries for a conditioned DEM based on specified points of interest}
#'  \item{ch_saga_channels}{Generates a drainage network from DEM}
#'  \item{ch_contours}{Creates contour lines from DEM}
#'  \item{ch_checkcatchment}{Provides a simple map to check the outputs from ch_saga_catchment}
#'  \item{ch_checkchannels}{Provides a simple map to check the outputs from ch_saga_channels}
#' }
NULL