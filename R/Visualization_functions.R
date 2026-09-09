#' Visualization functions
#' @name Visualization-functions
#' @description These functions are primarily intended for graphing, although
#' some analyses may also be done.
#' \describe{
#'  \item{ch_booth_plot}{Plot of peaks over a threshold}
#'  \item{ch_decades_plot}{Plots two decades of binned flows from \code{ch_binned_MannWhitney}}
#'  \item{ch_ffa_screen_plot}{FFA screening plot}
#'  \item{ch_flow_raster}{Raster plot of streamflows}
#'  \item{ch_flow_raster_qa}{Raster plot of streamflows with WSC quality flags}
#'  \item{ch_flow_raster_trend}{Raster plot of streamflows with binned trends}
#'  \item{ch_gg_hydrographs}{ggplot2 hydrographs of WSC flows}
#'  \item{ch_model_hydrograph}{Plots hydrographs and/or precipitation}
#'  \item{ch_polar_plot}{Polar plot of daily streamflows}
#'  \item{ch_polar_plot_peaks}{Polar plot of the timing of peak flows}
#'  \item{ch_polar_plot_prep}{Assembles a data structure for \code{ch_polar_plot}}
#'  \item{ch_qa_hydrograph}{Plots a hydrograph with the data quality symbols}
#'  \item{ch_regime_plot}{Plots the regime of daily streamflows}
#' }
#'
#' The following are helpers used by the plotting functions, and are also useful
#' when building plots directly:
#' \describe{
#'  \item{ch_axis_doy}{Draws a day-of-year or day-of-water-year axis}
#'  \item{ch_sub_set_Years}{Sub-samples a vector of labels so axis labels do not overlap}
#'  \item{ch_col_gradient}{Creates a colour gradient (red-white-blue by default)}
#'  \item{ch_color_gradient}{Creates a colour gradient (red-white-green by default)}
#'  \item{ch_col_transparent}{Adds transparency to a colour}
#'  \item{ch_circular_colors}{Creates a cyclic colour palette for circular plots}
#' }
NULL
