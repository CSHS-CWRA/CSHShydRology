% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch_checkchannels.R
\name{ch_checkchannels}
\alias{ch_checkchannels}
\title{Check Channels}
\usage{
ch_checkchannels(
  dem,
  channels,
  outlet = NULL,
  main_label = "",
  channel_colour = "blue",
  pp_colour = "red",
  contour_colour = "grey"
)
}
\arguments{
\item{dem}{raster DEM that catchments were generated from}

\item{channels}{channel polyline (or channels list from \code{ch_wbt_channels}) (sf object)}

\item{outlet}{location of catchment outlet (sf object)}

\item{main_label}{Main label for channel plot.}

\item{channel_colour}{Colour for channel. Default is "blue".}

\item{pp_colour}{Colour for catchment pour points. Default is "red".}

\item{contour_colour}{Colour for contours Default is "grey".}
}
\value{
\item{check_map}{a \pkg{ggplot} object of a map with channel layer}
}
\description{
Generates a map of the generated channel network layer.
}
\details{
Generates a simple map of the drainage network plotted over the contours to allow a visual assessment.
}
\examples{
# Only proceed if Whitebox executable is installed
library(whitebox)
if (check_whitebox_binary()){
  library(raster)
  test_raster <- ch_volcano_raster()
  dem_raster_file <- tempfile(fileext = c(".tif"))
  no_sink_raster_file <- tempfile("no_sinks", fileext = c(".tif"))

  # write test raster to file
  writeRaster(test_raster, dem_raster_file, format = "GTiff")

  # remove sinks
  removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, method = "fill")

  # get flow accumulations
  flow_acc_file <- tempfile("flow_acc", fileext = c(".tif"))
  flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)

  # get flow directions
  flow_dir_file <- tempfile("flow_dir", fileext = c(".tif"))
  flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
  channel_raster_file <- tempfile("channels", fileext = c(".tif"))
  channel_vector_file <- tempfile("channels", fileext = c(".shp"))
  channels <- ch_wbt_channels(flow_acc_file, flow_dir_file, channel_raster_file,
  channel_vector_file, 1)

  # get pour points
  pourpoint_file <- tempfile("volcano_pourpoints", fileext = ".shp")
  pourpoints <- ch_volcano_pourpoints(pourpoint_file)
  snapped_pourpoint_file <- tempfile("snapped_pourpoints", fileext = ".shp")
  snapped_pourpoints <- ch_wbt_pourpoints(pourpoints, flow_acc_file, pourpoint_file,
  snapped_pourpoint_file, snap_dist = 10)
  ch_checkchannels(test_raster, channels, snapped_pourpoints)
} else {
  message("Examples not run as Whitebox executable not found")
}
}
\seealso{
\code{\link{ch_checkcatchment}}
}
\author{
Dan Moore
}
