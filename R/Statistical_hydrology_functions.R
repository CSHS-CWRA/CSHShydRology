#' Statistical analysis functions
#' @name StatisticalHydrology-functions
#' @description These functions perform statistical analyses
#' \describe{
#'  \item{ch_binned_MannWhitney}{Compares two time periods of data using the Mann-Whitney test}
#'  \item{ch_fdcurve}{Finds flow exceedance probabilities and plots the flow duration curve}
#'  \item{ch_get_peaks}{Finds peak flows over a specified threshold}
#'  \item{ch_sh_get_amax}{Extracts the annual maximum series from a daily flow record}
#'  \item{ch_circ_mean_reg}{Finds the circular mean, median and regularity of event timing}
#'  \item{ch_high_Grubbs_test}{Screens the largest events for high outliers using the Grubbs test}
#'  \item{ch_slice}{Bins a day of year series into equal-width periods}
#'  \item{ch_tr_sign}{Converts trend slopes to direction indices}
#'  \item{ch_tr_signif}{Converts p-values to significance indices}
#' }
#'
#' Flood frequency analysis functions contributed by the FloodNet project are
#' prefixed \code{ch_rfa_} and are listed under \code{\link{Floodnet_functions}}.
NULL
